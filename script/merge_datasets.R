library(data.table)
source("./script/helpers/helpers.R")

# import data ---
# indexes
hpi = fread("./data/processed/HPI_fixed-effects_homes.csv", keepLeadingZeros = TRUE)
hri = fread("./data/processed/HRI_fixed-effects_aparts.csv", keepLeadingZeros = TRUE)

# distance zip-code to the cbd
dists = fread(
  "./data/geodata/zip-codes/zip-codes_dist2cbd.csv", keepLeadingZeros = TRUE
)

# cleaning ---
setnames(hpi, "eff", "val")
setnames(hri, "eff", "val")

## merge with dist ----
n_hpi = nrow(hpi)
n_hri = nrow(hri)
hpi = merge(hpi, dists, "unique_plz")
hri = merge(hri, dists, "unique_plz")
message(
  sprintf("For prices, %.2f%% of obs have been dropped.", 100 - 100 * nrow(hpi) / n_hpi)
)
message(
  sprintf("For rents, %.2f%% of obs have been dropped.", 100 - 100 * nrow(hri) / n_hri)
)


# bind price and rental indices
hedonic = rbindlist(
  list(lnhpi = hpi, lnhri = hri), use.names = TRUE, idcol = "var"
)
setcolorder(hedonic, "var", before = "val")
rm(hri, hpi)

hedonic[, time := mydate(mon, year)] # year-mon-01 for convenience
setcolorder(hedonic, "time", before = "var")


# add info from other data sets ----

## the 2019 municipality population ----
pop_mun = fread(
  "data/processed/population_municipality.csv",
  select = c("ags", "year", "pop_mun", "rank_pop"),
  key = c("ags", "year"), keepLeadingZeros = TRUE
)
pop_mun = pop_mun[year == 2019, ][, year := NULL]
pop_mun[, ags := padd_zero(ags, 8)]

hedonic = merge(hedonic, pop_mun, "ags", all.x = TRUE)


## the 2019 grid->zipcode pop and income, etc ----
## grid-zipcode-municipality link, for merging with the below data sets ----
xwalk_grids = fread(
  "./data/geodata/zip-codes/xwalk_grids-zipcodes-muns.csv",
  select = c("grid_id", "unique_plz"), keepLeadingZeros = TRUE
)
xwalk_grids = xwalk_grids[!unique_plz == "", ]

# includes control vars
microm = fread("data/processed/microm-tidy.csv") |>
  DT(year == 2019 & !is.na(inhabitants) & !is.na(purchase_power), !"year")

microm[, tot_purchase_power := purchase_power]
sum_vars = c("tot_purchase_power", "inhabitants", "num_hhs", "resid_buildings")
mean_vars = c("purchase_power", "unemp", "car_density", "share_background_german", "share_male_1845")
microm_mean = merge(xwalk_grids, microm, "grid_id")[, lapply(.SD, mean, na.rm = TRUE), .(unique_plz), .SDcols = c(mean_vars)]
microm_sum = merge(xwalk_grids, microm, "grid_id")[, lapply(.SD, sum, na.rm = TRUE), .(unique_plz), .SDcols = sum_vars]

microm = merge(microm_sum, microm_mean, "unique_plz")
rm(microm_sum, microm_mean)

# keep all cases in hedonic
hedonic = merge(hedonic, microm, "unique_plz", all.x = TRUE)

## covid cases -- district -----
# cases = readxl::read_excel('data/rki_data/infections_districts.xlsx', sheet='data')
# setDT(cases)
cases = fread("data/rki_data/2022-10-15_Deutschland_SarsCov2_Infektionen.csv")
cases = cases[, .(cases = sum(AnzahlFall, na.rm = TRUE)),
  .(time = as.Date(format(as.Date(Refdatum), "01-%m-%Y"), "%d-%m-%Y"),
    did = sprintf("%05i", IdLandkreis))
]

#  For a more precise representation of Berlin, the 12 city districts are
#  broken down as separate "counties".
cases = fread("data/rki_data/2022-10-15_Deutschland_SarsCov2_Infektionen.csv")
cases = cases[
  , .(cases = sum(AnzahlFall, na.rm = TRUE)),
  .(
    time = as.Date(format(as.Date(Refdatum), "01-%m-%Y"), "%d-%m-%Y"),
    did = sprintf("%05i", IdLandkreis)
  )
]

#  For a more precise representation of Berlin, the 12 city districts are
#  broken down as separate "counties".
berlin_counties = cases[did %like% "^11...$", which = TRUE]
berlin = cases[berlin_counties, .(did = "11000", cases = sum(cases, na.rm = TRUE)), time]

cases = rbind(cases[!berlin_counties, ], berlin, use.names = TRUE)

if ("did" %in% names(hedonic) && typeof(hedonic$did) == "integer") {
  hedonic[, did := sprintf("%05i", did)]
} else {
  hedonic[, did := sprintf("%05i", floor(as.integer(ags) / 1000L))]
}

hedonic = merge(hedonic, cases, c("did", "time"), all.x = TRUE)
pre_covid_time = min(cases$time)
# for years before the pandemic insert 0
hedonic[time < pre_covid_time, cases := 0]

setcolorder(
  hedonic,
  c("unique_plz", "plz", "ags", "geo_name", "did", "time", "mon", "year", "var", "val"),
)

## New variable for the indices ----
# smoothing and normalization
setkey(hedonic, unique_plz, var, time)
hedonic[, val_MA := log(frollmean(exp(val), n = 3, align = "center", na.rm = T)), .(unique_plz, var)
][, nat_ave_2019 := mean(exp(val[year(time) == 2019]), na.rm = TRUE), var
][, val_norm := log(exp(val) / nat_ave_2019)
][, val_MA_norm := log(exp(val_MA) / nat_ave_2019)
][, nat_ave_2019 := NULL]

## write to disk ----
fwrite(hedonic, "data/processed/final_indexes.csv")
