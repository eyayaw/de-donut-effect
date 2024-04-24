# This file generates descriptive stats for amenities.

library(data.table)
library(ggplot2)
library(sf)
library(fixest)
library(kableExtra)

source("./script/helpers/plot-helpers.R")
source("./script/helpers/helpers.R")

# default theme
theme_set(custom_theme_minimal(14))

# constants
source("./script/constants.R")
area_cov_max <- 20 # upper limit for avg_area_covered

# paths
main_path <- getwd()
data_path <- file.path(main_path, "data")
output_path <- file.path(main_path, "output")


#--------------------------------------------------
# load data

# dist to the cbd data
dists <- fread(
  file.path(data_path, "geodata/zip-codes/zip-codes_dist2cbd.csv"),
  drop = DROP_VARS
)

# normalized amenity data and housing data combined
housing_amenity <- fread(
  file.path(data_path, "processed/housing_amenities.csv"),
  drop = DROP_VARS
)

## amenities ----

# raw data
# c(
#     "geodata/amenities/amenities_combined_germany.gpkg",
#     "geodata/amenities/water_green_combined_germany.gpkg"
# ) |>
#    setNames(AMENITY_TYPES) |>
#     lapply(
#         \(f) st_drop_geometry(st_read(file.path(data_path, f), quiet = TRUE))
#     ) |>
#     rbindlist(idcol = "type", use.names = TRUE, fill = TRUE) |>
#     _[, !DROP_VARS, with = FALSE] -> amenities_raw

c(
  "geodata/amenities/amenities_combined_germany.csv",
  "geodata/amenities/water_green_combined_germany.csv"
) |>
  setNames(AMENITY_TYPES) |>
  lapply(\(f) fread(file.path(data_path, f), drop = DROP_VARS)) |>
  rbindlist(idcol = "type", use.names = TRUE, fill = TRUE) -> amenities_raw

# index data
c(
  "processed/cons_amenities_normalized_wide.csv",
  "processed/environ_amenities_normalized_wide.csv"
) |>
  setNames(AMENITY_TYPES) |>
  lapply(\(f) fread(file.path(data_path, f))) |>
  lapply(\(dt) melt(dt, measure.vars = patterns("_index$"), variable.factor = FALSE)) |>
  rbindlist(idcol = "type", fill = TRUE) -> amenities_index

# transform ----
setnames(dists, DIST_VAR, "dist")
dists[, lndist := log((1 + dist) / 1000)]
# drop other dist vars
dists[, grep("^dist_.+", names(dists), value = TRUE) := NULL]

setnames(housing_amenity, DIST_VAR, "dist")
housing_amenity[, lndist := log((1 + dist) / 1000)]
housing_amenity[, grep("^dist_.+", names(housing_amenity), value = TRUE) := NULL]

#--------------------------------------------------
# define decile of distances
# for determining CBD rings, and suburb area
# rule:
# 0-10%: CBD
# 10-30%: suburb ring 1
# 30-50%: suburb ring 2
# > 50%: outside
dist_quants <- quantile(dists$dist, probs = seq(0, 1, 0.1), na.rm = TRUE)
# classify zip codes
dists <- dists[
  ,
  cbd_suburb := fcase(
    dist <= dist_quants[["10%"]], "CBD",
    dist > dist_quants[["10%"]] & dist <= dist_quants[["30%"]], "suburb_ring_1",
    dist > dist_quants[["30%"]] & dist <= dist_quants[["50%"]], "suburb_ring_2",
    rep_len(TRUE, length(dist)), "outside"
  )
]

# add dist to the amenity data
housing_amenity <- merge(
  housing_amenity, dists[, c("unique_plz", "cbd_suburb")],
  by = "unique_plz", all.x = TRUE
)

#--------------------------------------------------
# summarise raw data per zipcode

cons_counts <- amenities_raw |>
  _[type == AMENITY_TYPES["cons"], .(N = .N), by = .(unique_plz, year, amenity_cat)] |>
  # add additional zipcode information
  merge(dists, by = c("unique_plz"), all.x = TRUE)


# summarise number of obs by zipcode
environ_counts <- amenities_raw |>
  _[type == AMENITY_TYPES["envt"],
    .(
      N = .N,
      N_by_people = .N / sum(einwohner, na.rm = TRUE),
      avg_area_covered = mean(get(grep("^covered", names(.SD), value = TRUE)))
    ),
    by = .(unique_plz, year)
   ] |>
  merge(dists, by = c("unique_plz"), all.x = TRUE)

#--------------------------------------------------
# plot raw data counts with respect to distance to CBD

# plot frequencies ----
## consumption amenities ----
cons_counts |>
  subset(dist <= DIST_MAX_METERS) |>
  {
    \(.x)
    ggplot(.x, aes(lndist, N)) +
      geom_point(alpha = 0.05, color = COLS[[1]]) +
      geom_smooth(method = "lm", col = COLS[[3]], linewidth = 1.25) +
      stat_summary_bin(
        fun = "mean", binwidth = binwidth_logscale(.x$lndist, 2),
        geom = "point", size = 2
      ) +
      labs(x = "Log distance to the CBD (km)", y = "Frequency")
  }() -> p

my_ggsave(
  file.path(output_path, "figs", paste0("frequency_CBD_distance_", AMENITY_TYPES["cons"], ".png")), p
)

## environmental amenities ---
# limit frequency to remove outliers
environ_counts |>
  subset(dist <= DIST_MAX_METERS) |>
  subset(N <= 2000) |> # this constant is okay: EB
  {
    \(.x)
    ggplot(.x, aes(lndist, N)) +
      geom_point(alpha = 0.1, col = COLS[[1]]) +
      geom_smooth(method = "lm", col = COLS[[3]], linewidth = 1.25) +
      stat_summary_bin(
        fun = "mean", binwidth = binwidth_logscale(.x$lndist, 2),
        geom = "point", size = 2
      ) +
      labs(x = "Log distance to the CBD (km)", y = "Frequency")
  }() -> p

my_ggsave(
  file.path(output_path, "figs", paste0("frequency_CBD_distance_", AMENITY_TYPES[["envt"]], ".png")), p
)


# plot area covered for environmental amenities
# limit area covered to remove outliers
environ_counts |>
  subset(dist <= DIST_MAX_METERS) |>
  subset(avg_area_covered <= (area_cov_max)) |>
  {
    \(.x)
    ggplot(.x, aes(lndist, avg_area_covered)) +
      geom_point(alpha = 0.1, col = COLS[[1]]) +
      geom_smooth(method = "lm", col = COLS[[3]], linewidth = 1.25) +
      stat_summary_bin(
        fun = "mean", binwidth = binwidth_logscale(.x$lndist, 2),
        geom = "point", size = 2
      ) +
      labs(
        x = "Log distance to the CBD (km)",
        y = "Avg. Area Covered (sq. km)"
      )
  }() -> p

my_ggsave(
  file.path(
    output_path, "figs", paste0("area_CBD_distance_", AMENITY_TYPES[["envt"]], ".png")
  ), p
)


#--------------------------------------------------
# descriptives for counts
## should we show the average or the total num of amenities?
cons_dstats <- cons_counts[!is.na(cbd_suburb), ] |>
  _[, .(N = mean(N, na.rm = TRUE), SD = sd(N, na.rm = TRUE)), cbd_suburb]

environ_dstats <- environ_counts[!is.na(cbd_suburb), ] |>
  _[
    , .(
      avg_area_covered = mean(avg_area_covered, na.rm = TRUE),
      SD = sd(avg_area_covered, na.rm = TRUE)
    ),
    cbd_suburb,
  ]

dstats <- merge(
  cons_dstats, environ_dstats,
  by = "cbd_suburb", suffixes = c("_cons", "_envt"), sort = FALSE
)
# underscore is a special char in latex
dstats[, cbd_suburb := trimws(gsub("_", " ", cbd_suburb))][, cbd_suburb := tools::toTitleCase(cbd_suburb)]

fwrite(
  dstats, file.path(output_path, "descriptives/descriptive-stats_amenities.csv")
)

# house keeping
rm(cons_dstats, environ_dstats)


#--------------------------------------------------
# plot indices across all times

# add distance to the index data
amenities_index <- merge(
  amenities_index, dists[, c("unique_plz", "lndist", "area_qkm"), with = FALSE], "unique_plz",
  all.x = TRUE
)

amenities_index[
  exp(lndist) <= DIST_MAX_METERS &
    variable %in% c("urbam_index", "environ_area_index"),
] |>
  {
    \(.x)
    split(.x, .x$variable)
  }() |>
  lapply(function(x) {
    ggplot(x, mapping = aes(lndist, value)) +
      geom_point(alpha = 0.1, col = COLS[[1]]) +
      geom_smooth(method = "lm", col = COLS[[3]], linewidth = 1.25) +
      labs(
        x = "Log distance to the CBD (km)",
        y = "Amenity index"
      )

    my_ggsave(
      file.path(output_path, "figs", paste0(x$variable[[1]], "_CBD_distance.png"))
    )
  })


#--------------------------------------------------
# plot indices with respect to hedonic values

# comparison between 2020 and 2021 across all zip codes independent of CBD/suburb
for (hed_var in HED_VARS) {
  for (amenity_var in AMENITY_VARS) {
    housing_amenity |>
      subset(time %in% TIMES & var == hed_var) |>
      transform(time = format.Date(time, "%b %Y")) |>
      ggplot(aes(val, .data[[amenity_var]], col = time)) +
      geom_point(alpha = 0.1) +
      geom_smooth(
        method = "lm", se = FALSE, linewidth = 1.25
      ) +
      scale_color_manual(
        values = setNames(COLS[seq_along(TIMES)], names(TIMES))
      ) +
      labs(x = get_label(hed_var), y = "Amenity index", col = "") +
      theme(legend.position = "bottom")

    my_ggsave(
      file.path(
        output_path, "figs", paste0(hed_var, "_", amenity_var, ".png")
      ),
      width = 6, height = 4.5
    )
  }
}

# comparison between 2020 and 2021 dependent on CBD/suburb
locs <- c("CBD", "Suburb")
# subset for suburbs combined (ring 1 + ring 2)
housing_amenity[, cbd_suburb2 := fcase(
  cbd_suburb %in% c("suburb_ring_1", "suburb_ring_2"), "Suburb",
  cbd_suburb == "CBD", "CBD",
  rep(T, length(cbd_suburb)), cbd_suburb
)]

for (loc in locs) {
  housing_amenity[,
    c("unique_plz", "time", "var", "val", "cbd_suburb2", AMENITY_VARS),
    with = FALSE
  ] |>
    subset(time %in% TIMES & cbd_suburb2 == loc) |>
    transform(time = format.Date(time, "%b %Y")) |>
    melt(measure.vars = AMENITY_VARS, value.name = "val_amen", variable.name = "var_amen") |>
    ggplot(aes(val, val_amen, col = time)) +
    geom_point(alpha = 0.1) +
    geom_smooth(
      method = "lm", se = FALSE, linewidth = 1.25
    ) +
    facet_wrap(
      var ~ var_amen,
      scales = "free", strip.position = "top",
      labeller = labeller(
        var = c(lnhpi = "Prices", lnhri = "Rents"),
        var_amen = c(urbam_index = "Cons. amenities", environ_area_index = "Envt. amenities")
      )
    ) +
    scale_color_manual(
      values = setNames(COLS[seq_along(TIMES)], names(TIMES))
    ) +
    labs(x = expression(ln ~ P * ", " * ln ~ R), y = "Amenity index", col = "") +
    theme(legend.position = "bottom", strip.placement = "outside")

  filename <- paste0("hedonic_vs_amenities_", loc, ".png")
  my_ggsave(file.path(output_path, "figs", filename), width = 13)
}


#--------------------------------------------------
# estimating the relationship between amenities and dist to the cbd
## i.e., estimate b and phi
# construct count and area cover by zip code
est_data_cons <- amenities_raw[
  year == 2019 & type == "consumption", .(cons_amen = .N), .(unique_plz)
]
est_data_envt <- amenities_raw[
  year == 2019 & type == "environmental",
  .(unique_plz, envt_amen = covered_water_green_qkm)
]

est_data <- merge(est_data_cons, est_data_envt) |>
  merge(dists[dist <= DIST_MAX_METERS, .(unique_plz, lndist)])

rm(est_data_cons, est_data_envt)

# regress environmental index on distance
feols(
  c(log(cons_amen), log(envt_amen)) ~ lndist,
  data = est_data, vcov = "hetero"
) -> mods_raw


amenities_index |>
  subset(year == 2019 & variable %in% c("urbam_index", "environ_area_index")) |>
  dcast(unique_plz + lndist ~ variable, value.var = "value") |>
  feols(
    c(urbam_index, environ_area_index) ~ lndist,
    data = _, vcov = "hetero"
  ) -> mods_index

# labels for the variables
dict <- c(
  "log(cons_amen)" = "$\\ln \\text{count}$",
  "log(envt_amen)" = "$\\ln\\text{area km}^2$",
  "lndist" = "$\\ln \\text{dist}$",
  "urbam_index" = "\\text{index}",
  "environ_area_index" = "\\text{index}"
)

# export
etable(
  c(mods_raw, mods_index)[c(1, 3, 2, 4)],
  tex = TRUE,
  fitstat = c("n", "r2"),
  headers = list(
    "^:_:Type" = list("**Consumption amenities**" = 2, "**Environmental amenities**" = 2)
  ),
  dict = dict,
  style.tex = style.tex("aer"),
  file = file.path(output_path, "tabs/parameter_estimates_amenities.tex"),
  notes = "\\textit{Notes:} The table shows the estimates for the parameters $b$ and $\\phi$ from Equation (\\ref{eq-amenities}), which represent the relationship between amenities and the distance to the CBD. The estimation uses amenity data for 2019. The number of observations for the indices represents zip codes. In the raw data, the number of observations counts the amenity object.",
  tpt = TRUE,
  float = TRUE,
  placement = "",
  # title = "Estimates for the parameters $b$ and $\\phi$",
  # label = "tbl-parameter-estimates-amenities",
  replace = TRUE,
  postprocess.tex = \(x) sub(r"(\\caption\{.*\})", "", x)
)


#--------------------------------------------------
# estimating the effect of the pandemic on amenity valuation

# transform to log scale
housing_amenity[, `:=`(
  lndist = log(1 / 1000 + dist / 1000),
  lnpop_dens = log(pop_mun / area_qkm),
  lnpop = log(1 + inhabitants),
  lnpp = log(1 + tot_purchase_power)
)]

# create a pandemic dummy
housing_amenity[, pandemic := as.factor(fifelse(time >= TIMES[["Mar 2020"]], 1, 0))]

# add month-year variable
housing_amenity[, year_mon := as.factor(format(time, "%Y-%m"))]

# define controls
controls <- c("lnpp", "share_male_1845", "share_background_german")

# select amenity index variables to iterate over
index_vars_all <- grep(
  "(?<!people)_index$", names(housing_amenity),
  perl = T, value = T,
)
# keep a subset of vars needed for estimation
selected <- c(
  "unique_plz", "time", "year_mon", "var", "val_MA", "area_qkm", "amr_name", "lndist", "pandemic", controls, index_vars_all
)
# wide format
mod_data <- housing_amenity[, ..selected] |>
  # amenity data available from 2017 onwards
  subset(year(time) >= 2017) |>
  dcast(... ~ var, value.var = "val_MA")

lhs <- sprintf("c(%s)", paste(HED_VARS, collapse = ","))
mods <- vector("list", length(index_vars_all))
names(mods) <- index_vars_all

for (x in index_vars_all) {
  fml <- sprintf(
    "%s ~ %s * pandemic + %s | amr_name", lhs, x, paste(controls, collapse = "+")
  )

  mods[[x]] <- feols(as.formula(fml), data = mod_data, vcov = "hetero")
}


# export model results
for (hed_var in HED_VARS) {
  esttex(
    # special fixest method to get model results by the lhs var
    lapply(mods, \(mod) mod[lhs = hed_var]),
    headers = hed_var,
    replace = TRUE,
    file = file.path(
      output_path, "tabs",
      paste0("all", "_amenity_pandemic_effect_", hed_var, ".tex")
    )
  )
}

# Note: it would great to run the above estimation with the count data.

# estimation for sub-samples: distance rings: # todo
