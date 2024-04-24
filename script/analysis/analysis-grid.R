library(data.table)
library(sf)
library(units)
library(ggplot2)
source('script/helpers/helpers.R')

## to do
# visualization -----
# 1. within cities (distance groups) and across cities (large vs others)
  # a. hpi
  # b. rent
  # c. number of sells or ads


# estimation ----

# 1. grid level
# 2. zip code level ?
# 2. city level


# grid level ----
## import data ----
keys = c('grid_id', 'ars', 'ags', 'geo_name')
de_grid = st_read('data/processed/geodata/grid-germany.gpkg')
de_grid$id = NULL
st_geometry(de_grid) = 'geometry'

hedonic = fread('data/processed/final_indexes_grid.csv')
setcolorder(hedonic, 'time', before = 'year')

hedonic = merge(hedonic[, !c('did')], st_drop_geometry(de_grid), 'grid_id')
setcolorder(hedonic, c(keys, 'zipcode', 'did', 'district_name'))

## grid with very few occurrence
rare = hedonic[, .(pos = .I[.N < 2]), .(grid_id, var)]$pos
hedonic = hedonic[-rare, ]
rm(rare)


# create groups ----
## get the 2019 municipality population
pop_mun = fread(
  "../../../data/processed/population_municipality.csv",
  select = setNames(
    c(rep("character", 3), rep("integer", 2)),
    c("ars", "ags", "geo_name", "year", "pop_tot")
  ),
  key = c("ags", "year"), keepLeadingZeros = TRUE
)
pop_mun = tidyr::complete(pop_mun,
  year = seq(min(pop_mun$year), max(pop_mun$year), 1),
  ags = unique(pop_mun$ags)
)
setDT(pop_mun)
pop_mun = pop_mun[, pop_tot := nafill(nafill(pop_tot, "locf"), "nocb"), ags]
pop_mun = pop_mun[!is.na(pop_tot), ] # uninhabited
pop_mun[, rank_pop := frank(-pop_tot, ties.method = "dense"), year]
pop_mun = pop_mun[year==2019, !'year']

# add population data and filter the largest 20
hedonic = hedonic[pop_mun[, .(ags, ars, pop_mun = pop_tot, rank_pop)], on = c("ags", "ars"), nomatch = NULL]
setcolorder(hedonic, c(keys))

hedonic_20 = hedonic[rank_pop <= 20, .(grid_id, ags, time, var, val, nads, inhabitants)]

## get the 2019 grid population, and dist to cbd for the largest 20
dist = fread('data/processed/distance_grid-cbd.csv', drop = 'amr_id')
microm_2019 = fread("data/processed/microm-tidy.csv",
                    select = c("grid_id", "year", "inhabitants")
                    )[year == 2019 & !is.na(inhabitants), !'year']
microm_2019 = merge(microm_2019, dist, 'grid_id')
rm(dist)
microm_2019 = merge(microm_2019, st_drop_geometry(de_grid)[, c(keys)], "grid_id") |>
  merge(pop_mun[, .(ags, pop_tot, rank_pop)], "ags")

microm_2019 = microm_2019[rank_pop <=20, ]


## create density by population density
# construct groups by population density or presence in the cbd
# we define the area of a CBD to be all grid cells with centroids within k kms
# of the CBD coordinates.
# define the cbd, and the other 3 groups: low, mid, and top density
# 50th and 90th percentiles

cbd_radius = units::set_units(5000, 'm')
microm_2019[, dist_grid_cbd := units::set_units(dist_grid_cbd, 'm')]
ptiles = microm_2019[, quantile(inhabitants, probs = c(.5, .9), na.rm = TRUE)]
microm_2019[, density := fcase(
  # dist_grid_cbd < cbd_radius, "cbd",
  inhabitants < ptiles["50%"], "low",
  inhabitants >= ptiles["50%"] & inhabitants < ptiles["90%"], "mid",
  inhabitants >= ptiles["90%"], "high",
  rep(TRUE, length(inhabitants)), NA_character_
)]
stopifnot(microm_2019[is.na(density), .N] == 0L) # no need, but just in case


## create distance groups
# cutoffs = units::set_units(c(1000 * c(0, 4, 8, 12), Inf), 'm')
cutoffs = units::set_units(c(1000 * c(0, 5, 10), Inf), 'm')
labs = c('<5km', '5-10km', '>10km')
microm_2019[, ring := my_cut(dist_grid_cbd, cutoffs, labels = labs)]
setcolorder(microm_2019, c(keys, 'density', 'ring'))

hedonic_20 = merge(hedonic_20, microm_2019[, .(grid_id, density, ring)], 'grid_id')


## remove seasonality -- smoothing
# .start = c(2018L, 1L)
# .end = c(2021L, 12L)
# hedonic_stl = hedonic_20[, .(grid_id, year, mon, var, val)] |>
#   split(by = c('grid_id', 'var')) |>
#   lapply(\(x) decomp(x, "trend", .start, .end)) |>
#   rbindlist(use.names = TRUE, idcol = 'grid.var')
# setnames(hedonic_stl, c('val', 'stlval'), c('oval','val'))
# hedonic_stl[, c('grid_id', 'var') := tstrsplit(grid.var, '\\.', type.convert = FALSE)]
# hedonic_20 = merge(hedonic_20, hedonic_stl[, !'grid.var'], c('grid_id', 'year', 'mon', 'var'))


# stack the two group sets together
hedonic_20 = melt(hedonic_20,
  measure.vars = c("density", "ring"), variable.name = "type",
  value.name = "group", value.factor = FALSE
)


### aggregate by groups ----
hedonic_groups = hedonic_20[, .(
  #oval = weighted.mean(oval, inhabitants, na.rm = TRUE),
  val = weighted.mean(val, inhabitants, na.rm = TRUE),
  nads = sum(nads, na.rm = TRUE),
  inhabitants = sum(inhabitants, na.rm = TRUE)
),
.(var, time, type, group)
]

## smooth by a 3-month moving average
## for each group for each year
win = 3
align = 'center'
hedonic_groups = hedonic_groups[
  order(var, type, group, time),
  .(time, inhabitants, nads, val, ma = frollmean(val, n=win, align=align),
    fma = zoo::na.approx(frollmean(val, n=win, align=align), na.rm=FALSE, rule=2L)),
  .(var, type, group, year = year(time)),
]


# remove seasonality once more
# hedonic_groups = hedonic_groups[, !c('nads', 'inhabitants')] |>
#   split(by = c("var", "group")) |>
#   lapply(\(x) merge(x, decomp(x[, .(year, mon, val)], "trend"), c("year", "mon"))) |>
#   rbindlist(use.names = TRUE)
#
# setcolorder(hedonic_groups, c('var', 'type', 'group', 'year', 'mon', 'time'))
# setnames(hedonic_groups, c('val', 'stlval'), c('ooval','val'))

### visualization ----
# begining of covid
ref_mon = 3
ref_year = 2020
ref_line = as.Date(sprintf("%s-%02i-01", ref_year, ref_mon))

hedonic_groups[,.(time, val = fma / fma[time == ref_line]), .(var, type, group)] |>
  subset(time > '2019-10-01') |>
  ggplot() +
  geom_line(aes(time, val, color = group)) +
  geom_vline(xintercept = ref_line) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  facet_wrap(~var+type, scales = "free_y") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

hedonic_groups[,.(time, val = ma / ma[time == ref_line]), .(var, type, group)] |>
  subset(time > '2019-10-01' & var == "lnhpi" & type == "ring") |>
  ggplot() +
  geom_line(aes(time, val, color = group)) +
  geom_vline(xintercept = ref_line) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))
