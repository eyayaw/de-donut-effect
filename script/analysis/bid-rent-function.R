library(sf)
library(ggplot2)
library(cowplot)
library(fixest)
library(data.table)

source('script/helpers/helpers.R')
# mon-year to date
mydate <- function(x) {
  as.Date(paste0("01-", x), "%d-%m-%Y")
}

getLabel <- function(x) {
  switch(x, lnp = "Prices (log)", lnr = "Rents (log)", x) #no style
}


# bid-rent functions -----------------------------------------------------------
## rental ----
hri = fread("data/processed/HRI_exact-geoloc_all-homes.csv", keepLeadingZeros = TRUE)
# time index: mon-year
hri[, my := as.factor(sprintf("%s-%i", mon, year))
    ][, amr_id := as.factor(amr_id)
      ][, `:=`(
  lndist = log(dist_grid2cbd + 1),
  lnpop = log(inhabitants),
  lnpp = log(purchase_power)
)]

model_hri = feols(lnhri ~ 0 + my:lndist + lnpop + lnpp | my + amr_id, hri, combine.quick = FALSE)

## prices ----
hpi = fread("data/processed/HPI_exact-geoloc_homes.csv", keepLeadingZeros = TRUE)
# time index: mon-year
hpi[, my := as.factor(sprintf("%s-%i", mon, year))
    ][, amr_id := as.factor(amr_id)
      ][, `:=`(
  lndist = log(dist_grid2cbd + 1),
  lnpop = log(inhabitants),
  lnpp = log(purchase_power)
)]

model_hpi = feols(lnhpi ~ 0 + my:lndist + lnpop + lnpp | my + amr_id, hpi, combine.quick = FALSE)

## combine ----
model = list(lnhri = model_hri, lnhpi = model_hpi)
# get the bid-rent coefficients
bidrent = lapply(model, \(m) m$coeftable[, "Estimate", drop = FALSE]) |>
  lapply(\(x) data.frame(
    my = sub("my(\\d{2}-\\d{4})\\:.+", "\\1", rownames(x)),
    estimate = x$Estimate
  )) |>
  setNames(names(model)) |>
  rbindlist(use.names = TRUE, idcol = "variable") |>
  subset(my %like% "^\\d\\d-\\d{4}$")

cis = lapply(model, \(m) confint(m)) |>
  lapply(\(.x) data.frame(my = sub("my(\\d{2}-\\d{4})\\:.+", "\\1", rownames(.x)), .x)) |>
  lapply(setNames, nm = c("my", "ll", "ul")) |>
  setNames(names(model)) |>
  rbindlist(use.names = TRUE, idcol = "variable") |>
  subset(my %like% "^\\d\\d-\\d{4}$")

bidrent = merge(bidrent, cis, c("my", "variable"))
fwrite(bidrent, "output/bidrent_grid.csv")

ggplot(bidrent[variable == "lnhri", ], aes(mydate(my), estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = ll, ymax = ul)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%m-%Y") +
  labs(x = "time", y = expression(Estimate ~ (widehat(delta)) ~ and ~ "95%" ~ CI)) +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

# descriptive evidence --------------------------------------------------------
grid_admin = fread("../Common-Data/.GRID_v11/Zuordnung_Gemeinden/2020_Grids_Municipality_Exact_unambiguous.csv", keepLeadingZeros = TRUE)
setnames(grid_admin, c("r1_id", "AGS", "share"), c("grid_id", "ags", "grid_share"))
grids = st_read('../Common-Data/.GRID_v11/Raster_shp/ger_1km_rectangle.shp')[, c("idm", "geometry")] |> setNames(c('grid_id', 'geometry'))

grids = merge(grids, grid_admin, 'grid_id') # for admin ids
cbds = st_read("data/geodata/CBDs/CBD_AMR_EK_COM.gpkg") |>
  st_transform(st_crs(grids))


## bid-rent function ----------------
hri[year %in% c(2020, 2021) & mon == "02" & !is_outlier(lnhri), ] |>
  ggplot(aes(lndist, lnhri, color = my)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("skyblue", "seagreen")) +
  theme_cowplot() +
  facet_wrap(~year) +
  labs(x = expression(ln(plain(distance) + 1)), color = NULL) +
  theme(legend.position = c(0.8, 0.9))


## maps --------------

hri_geo = merge(grids, hri, 'grid_id')

selected = c(
  berlin = "11000",
  hamburg = "02000",
  munich = "09184",
  frankfurt = "06412",
  cologne = "05315",
  bremen = "04011",
  dortmund = "05913",
  essen = "05113",
  duisburg = "05112"
)
measure = "lnhri"
out = vector("list", length(selected))
names(out) = names(selected)

for (i in seq_along(selected)) {
  hri_geo |>
    subset(substr(ags, 1,5)== selected[[i]] & year %in% c(2019, 2020, 2021) & mon %in% "12",
      select = c("year", measure, "ags", 'amr_id')
    ) -> d
  amr_id = unique(d[substr(d$ags, 1,5) == selected[[i]], ]$amr_id)
  out[[i]] = ggplot() +
    geom_sf(aes(fill = get(measure)), data = d) +
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
    geom_sf(fill = NA, lwd = 0.5, color = "blue", data = cbds[cbds$LLM_EK == amr_id,]) +
    geom_sf(fill = NA, lwd = 2.5, color = "red", data = st_centroid(cbds[cbds$LLM_EK == amr_id, ])) +
    labs(
      title = sprintf(
        "%s in December (%s)",
        getLabel(measure), tools::toTitleCase(names(selected)[i])
      ),
      fill = measure
    ) +
    cowplot::theme_map() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    )
}

for (i in seq_along(selected)) {
  ggsave(sprintf("doc/figs/%s_%s_grid.png", names(selected)[[i]], measure), out[[i]], width = 8, height = 5)
}
