library(sf)
library(ggplot2)
library(cowplot)
library(fixest)
library(data.table)
source('script/helpers/helpers.R')

# append 01 to mon-year -> date
mydate <- function(x) {
  as.Date(paste0("01-", x), "%d-%m-%Y")
}

getLabel <- function(x, math=FALSE) {
  switch(x,
    lnp   = if (math) expression(ln~P) else "Prices (log)",
    lnr   = if (math) expression(ln~R) else "Rents (log)",
    lnhpi = if (math) expression(ln~P) else "Prices (log)",
    lnhri = if (math) expression(ln~R) else "Rents (log)",
    x
  )
}
start_mon = 2 # pandemic cutoff
since = 2018
rewrite = TRUE


# import ----
# bid-rent functions -----------------------------------------------------------
hpi = fread("data/processed/HPI_all-homes.csv")[year >= (since)]
hri = fread("data/processed/HRI_zipcode_flats.csv")[year >= (since), ]
setnames(hpi, "eff",  "val")
setnames(hri,  "eff", "val")

dist_cbd = fread('data/processed/distance_zip-codes_LMRs.csv',
                 select = c('zipcode', 'did', 'amr_name', 'amr_id', 'dist'))

## controls ----
grid_admin = fread("../Common-Data/.GRID_v11/Zuordnung_Gemeinden/2020_Grids_Municipality_Exact_unambiguous.csv", keepLeadingZeros = TRUE)
setnames(grid_admin, c("r1_id", "AGS", "share"), c("grid_id", "ags", "grid_area_share_in_ags"))
if (grid_admin[, .N, .(l = nchar(ags))]$l == 8) {
  grid_admin[, did := as.integer(substr(ags, 1, 5))]
} else {
  stop("AGS should be a 8 character length vector", call. = FALSE)
}
microm = fread('data/processed/microm-tidy_2005-2019.csv')[year==2019,
    !(share_male_1820:share_male_4045)
    ][, year := NULL][]

microm = grid_admin[microm, on='grid_id', nomatch=NULL]
microm = microm[dist_cbd[, !"dist"], on='zipcode']

microm_zip = microm[
  , .(
    pop = sum(inhabitants, na.rm = TRUE),
    num_hhs = sum(num_hhs, na.rm = TRUE),
    buildings = sum(resid_buildings, na.rm = TRUE),
    ppower = mean(purchase_power,  na.rm = TRUE),
    unemp = mean(unemp, na.rm = TRUE),
    car_density = mean(car_density, na.rm = TRUE),
    share_background_german = mean(share_background_german, na.rm = TRUE),
    share_male_1845 = mean(share_male_1845, na.rm = TRUE)
  ),
  .(zipcode, did, amr_id)
]

rm(grid_admin)
microm_zip = merge(microm_zip, dist_cbd)


# create 4 groups
cbd_radius = 2000
ptiles = microm_zip[, quantile(pop, c(.5, .9), na.rm = TRUE)]
microm_zip[, group := fcase(
  dist < cbd_radius, "cbd",
  pop < ptiles["50%"], "low",
  pop >= ptiles["50%"] & pop < ptiles["90%"], "mid",
  pop >= ptiles["90%"], "high",
  rep(TRUE, length(pop)), NA_character_
)]
# rank of lmrs by population size
microm_zip[, let(tot_pop = sum(pop, na.rm = TRUE)), .(amr_id)
          ][, rank_lmr := frank(-tot_pop, ties.method = "dense")
          ][, tot_pop := NULL]

## stack indices ------------------
hi = list(lnhpi = hpi, lnhri = hri) |>
  rbindlist(use.names = TRUE, idcol = "var") |>
  merge(microm_zip[!is.na(group), ], c("zipcode", "did"), all.x = TRUE)
setcolorder(
  hi,
  c("zipcode", "did", "amr_id", "amr_name", "year", "mon", "group", "var")
)

# problematic (most probably wrong postcode entry by rwi/immoscout)
# .(.N, did = list(did))
# dups = hi[, .N, .(zipcode, year, mon, var)][N != 1, unique(zipcode)]
# hi[zipcode %in% dups, .SD[!(is.na(amr_id) | is.na(dist))], var][, .N, .(zipcode, mon, year, var)][N>2, ]

hi = hi[!is.na(amr_id), ]

if (rewrite) fwrite(hi, "data/processed/merged_prices-rents_zip-code.csv")

### stl decomposition ----

# toy_zip = hi[var == "lnhri" & zipcode == 45145, val, keyby = .(year, mon)]

decomp = function(x, return = c("trend", "seasonal", "detrended", "deseasoned")) {
  stopifnot(all(c("year", "mon", "val") %in% names(x)))
  return = match.arg(return)
  # x = x[order(x$year, x$mon),] # tidyr::completes does ordering automatically
  xc = tidyr::complete(x, tidyr::expand(x, year = (2018:2021), mon = (1:12)))
  actual = ts(xc$val,
    start = c(2018, 1),
    end = c(2021, 12),
    frequency = 12
  )
  # deal with NA actual and NA introduced by tidy::complete
  seen = -(setdiff(which(is.na(xc$val)), which(is.na(x$val))))
  if (length(seen) == 0) {
  seen = seq_len(nrow(xc))
}
  .na.action = \(x) zoo::na.approx(x, na.rm = FALSE, rule = 2)
  tryCatch({
    components = stl(actual, s.window = "per", na.action = .na.action)$time.series
    res = switch(return,
      trend = components[, "trend"],
      seasonal = components[, "seasonal"],
      deseasoned = actual - components[, "seasonal"],
      detrended = actual - components[, "trend"],
      stop('Did you pass the right val for `return`?', call. = FALSE)
    ); res[seen]}, error = function(e) {
    withCallingHandlers({warning(e); xc$val[seen]})
  })
}

vars = c("lnhpi", "lnhri")
hi_stl = vector("list", length(vars))
names(hi_stl) = vars
for (v in seq_along(vars)) {
  zips = unique(hi[var == (vars[[v]]), ]$zipcode)
  hi_stl[[v]] = vector("list", length(zips))
  names(hi_stl[[v]]) = as.character(zips)
  for (g in seq_along(zips)) {
    hi_stl[[v]][[g]] = hi[var == (vars[[v]]) & zipcode == zips[[g]], .(year, mon, val)
    ][order(year, mon), ] |>
      (\(d) cbind(d,
        val_trend = decomp(d, "trend"),
        val_deseasoned = decomp(d, "deseasoned")
      ))()
  }
}

hi_stl = lapply(hi_stl, rbindlist, use.names = TRUE, idcol = 'zipcode') |>
  rbindlist(use.names = TRUE, idcol = 'var')

hi_stl[, zipcode := as.integer(zipcode)]
hi = merge(hi, hi_stl[, !'val'], c('var', 'zipcode', 'year', 'mon'), all.x = TRUE)

qvars = c('val', 'val_trend', 'val_deseasoned')
hi_bucket = hi[, lapply(.SD, \(x) weighted.mean(x, pop, na.rm = TRUE)),
                   .(var, group, year, mon), .SDcols = (qvars)]
hi_bucket[, sub('val', 'ind', (qvars)) :=
            lapply(.SD, \(x) x / x[year == 2020 & as.integer(mon) == start_mon]),
          .(var, group), .SDcols = (qvars)]

# decompse to trend/seasonal
# hi_bucket_stl =
#   hi_bucket[, .(var, group, year, mon = as.integer(mon), val)] |>
#   split(by = "var") |>
#   lapply(split, by = "group") |>
#   lapply(\(ldf) lapply(ldf, \(x)  x[,.(year, mon, val)][order(year, mon), ] |>
#     (\(d) cbind(d,
#       val_trend = decomp(d, "trend"),
#       val_deseasoned = decomp(d, "deseasoned")
#     ))())) |>
#   lapply(rbindlist, use.names = TRUE, idcol = "group") |>
#   rbindlist(use.names = TRUE, idcol = "var")

vars = c("lnhpi", "lnhri")
hi_bucket_stl = vector("list", length(vars))
names(hi_bucket_stl) = vars
for (v in seq_along(vars)) {
  grps = unique(hi_bucket[var == (vars[[v]]), ]$group)
  hi_bucket_stl[[v]] = vector("list", length(grps))
  names(hi_bucket_stl[[v]]) = as.character(grps)
  for (g in seq_along(grps)) {
    hi_bucket_stl[[v]][[g]] = hi_bucket[var == (vars[[v]]) & group == grps[[g]], .(year, mon, val) #val= val_deseasoned
    ][order(year, mon), ] |>
      (\(d) cbind(d,
                  val_trend = decomp(d, "trend"),
                  val_deseasoned = decomp(d, "deseasoned")
      ))()
  }
}

hi_bucket_stl = lapply(hi_bucket_stl, rbindlist, use.names = TRUE, idcol = 'group') |>
  rbindlist(use.names = TRUE, idcol = 'var')

hi_bucket_stl[, (qvars) := lapply(.SD, nafill, type='locf'),
                     .(var, group), .SDcols = qvars]
hi_bucket_stl[, (qvars) := lapply(.SD, nafill, type='nocb'),
                     .(var, group), .SDcols = qvars]
hi_bucket_stl[, sub('val', 'ind', (qvars)) :=
            lapply(.SD, \(x) x / x[year == 2020 & as.integer(mon) == start_mon]),
          .(var, group), .SDcols = (qvars)]


hi_bucket = merge(hi_bucket, hi_bucket_stl[, !"val"],
  c("var", "group", "year", "mon"),
  suffixes = c("", "_stl"), all.x = TRUE
)

if (rewrite) fwrite(hi_bucket,
                    'data/processed/hedonic-index_density-group_zip-code.csv')

## estimation -----
hi[, lndist := log(dist/1000 + 1)][, dist := NULL] # avoid -Inf
hi[, my := as.factor(sprintf("%02i-%i", as.integer(mon), year))] # time index: mon-year
hi[, amr_name := as.factor(amr_name)]

# keep the largest 15 labor market regions
nlargest = 141
model = lapply(
  split(hi[rank_lmr <= nlargest, ], by = 'var'),
  # split(hi, by='var'),
  \(d) feols(
    val_deseasoned ~ 0 + my:lndist + log(pop) + log(ppower) | my + amr_name,
    data = d, combine.quick = FALSE
  )
)

# get the bid-rent coefficients
bidrent = lapply(model, \(m) m$coeftable[, "Estimate", drop = FALSE]) |>
  lapply(\(x) data.frame(
    my = sub("my(\\d{2}-\\d{4})\\:.+", "\\1", rownames(x)),
    estimate = x$Estimate
  )) |>
  setNames(names(model)) |>
  rbindlist(use.names = TRUE, idcol = "var") |>
  subset(my %like% "^\\d\\d-\\d{4}$")

cis = lapply(model, \(m) confint(m)) |>
  lapply(\(.x) data.frame(
    my = sub("my(\\d{2}-\\d{4})\\:.+", "\\1", rownames(.x)), .x)) |>
  lapply(setNames, nm = c("my", "ll", "ul")) |>
  setNames(names(model)) |>
  rbindlist(use.names = TRUE, idcol = "var") |>
  subset(my %like% "^\\d\\d-\\d{4}$")

bidrent = merge(bidrent, cis, c("my", "var"))

bidrent |>
  transform(my = mydate(my)) |>
  ggplot(aes(my, estimate)) +
  geom_line(color = 'darkred', lty=2) +
  geom_vline(xintercept = as.Date(paste0("2020-", start_mon, "-01"))) +
  geom_point() +
  geom_errorbar(aes(ymin = ll, ymax = ul)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  facet_wrap(~var, scales = 'free_y') +
  labs(x = "time", y = expression(Estimate ~ (widehat(delta)) ~ and ~ "95%" ~ CI)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

if (rewrite) {
  fwrite(bidrent,
  sprintf("output/bidrent-estimates_zip-code_%i-largest_lmrs.csv", nlargest))
}
# descriptive evidence --------------------------------------------------------

hi_bucket_stl |>
  ggplot() +
  geom_line(aes(as.Date(paste0(year, "-",mon, "-01")), ind_deseasoned, color = group), lwd = 1.1) +
  labs(x = NULL, color = NULL) +
  geom_vline(xintercept = as.Date(paste0("2020-", start_mon, "-01"))) +
  scale_color_manual(values = setNames(
    c("black", "forestgreen", "darkred", "darkblue"),
    c("low", "mid", "high", "cbd")
  )) + facet_wrap(~var, scales = 'free_y') +
  theme_classic(14) +
  theme(legend.position = c(.7, .2)) +
  guides(color = guide_legend(ncol=1))



# year-over-year changes
hi[
  ,
  .(
    d2020_2019 = val[year == 2020 & mon == start_mon] - val[year == 2019 & mon == start_mon],
    d2021_2019 = val[year == 2021 & mon == start_mon] - val[year == 2019 & mon == start_mon],
    d2021_2020 = val[year == 2021 & mon == start_mon] - val[year == 2020 & mon == start_mon]
  ),
  .(var, zipcode, did)
] -> changes


## bid-rent function ----------------
hi[year %in% c(2019, 2021) & as.integer(mon) == start_mon, .SD[!is_outlier(val)], var] |>
  ggplot(aes(lndist, val_trend, color = my)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("forestgreen", "darkred")) +
  facet_wrap(~var, scales = "free_y") +
  theme_cowplot() +
  labs(x = expression(ln(plain(distance) + 1)), color = NULL) +
  theme(legend.position = c(0.8, 0.9))


## maps --------------

zips = st_read("data/processed/geodata/zip-codes_tidy.gpkg")
cbds = cbds = st_read("data/geodata/CBDs/CBD_AMR_EK_COM.gpkg") |>
  st_transform(st_crs(zips))
st_geometry(cbds) = 'geometry'
names(cbds)[match('LLM_EK', names(cbds))] = 'amr_id'

cbds = merge(cbds, unique(st_drop_geometry(zips)[, c('did', 'amr_id')]))

hi_geo = merge(zips, hi, c("zipcode", "did"))

selected = list(
  berlin = "11000",
  hamburg = "02000",
  munich = "09184",
  frankfurt = "06412",
  cologne = "05315",
  bremen = "04011",
  dortmund = "05913",
  essen = "05113",
  duisburg = "05112"
) |> lapply(as.integer)

measures = c("lnhri", "lnhpi")
out = vector("list", length(selected))
names(out) = names(selected)

for (measure in measures) {
  for (i in seq_along(selected)) {
  hi_geo |>
    subset(var == measure & did == selected[[i]] & year %in% c(2019, 2020, 2021) & mon %in% start_mon,
      select = c('val', "year")
    ) -> d
  out[[i]] = ggplot() +
    geom_sf(aes(fill = val), data = d) +
    scale_fill_viridis_c(
      direction = -1L,
      guide = guide_colorbar(
        # direction = "vertical",
        # barheight = unit(.5, "cm"),
        barwidth = unit(.55, "cm"),
        label.theme = element_text(size = 9),
        # label.vjust = 2,
        title.position = "top",
        # title.vjust = .75,
        title.theme = element_text(size = 11)
      )
    ) +
    facet_grid(~year) +
    geom_sf(fill = NA, lwd = 0.5, color = "blue", data = cbds[cbds$did == selected[[i]], ]) +
    geom_sf(fill = NA, lwd = 2.5, color = "red", data = st_centroid(cbds[cbds$did == selected[[i]], "geometry"])) +
    labs(
      title = sprintf(
        "%s in %s (%s)",
        getLabel(measure), month.name[start_mon], tools::toTitleCase(names(selected)[i])
      ),
      fill = getLabel(measure, math = TRUE)
    ) +
    cowplot::theme_map() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    )
}

for (i in seq_along(selected)) {
  ggsave(sprintf("doc/figs/%s_%s.png", names(selected)[[i]], measure), out[[i]], width = 8, height = 5)
}
}
