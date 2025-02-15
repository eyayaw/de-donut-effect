library(data.table)
library(sf)
source('script/helpers/helpers.R') # for count_missing()

apart_rents = fread("data/apartment-rents_2007-2021.csv")
home_rents = fread("data/home-rents_2007-2021.csv")

# rm duplicates
keys = c("obid", "year", "ad_begin_mon")

## homes
nh = nrow(home_rents)
# set key for reordering the data.table in RAM by reference
setkeyv(home_rents, c(keys, "spell")) # spell is chronologically ordered
# get all (likely) duplicates with `dupID_gen == 1`
idx = home_rents[dupID_gen == 1, which = TRUE]
# keep the latest ad for the likely duplicates
dups = home_rents[idx, ]
dups = dups[dups[, .(idx = .I[which.max(spell)]), .(obid, year)]$idx,]

home_rents = rbind(home_rents[-idx, ], dups, use.names = TRUE)
message(sprintf("%.2f%% were duplicates.", 100-100*nrow(home_rents)/nh))
rm(dups, idx)

## apartments
na = nrow(apart_rents)
setkeyv(apart_rents, c(keys, "spell"))
idx = apart_rents[dupID_gen == 1, which = TRUE]
dups = apart_rents[idx, ]
dups = dups[dups[, .(idx = .I[which.max(spell)]), .(obid, year)]$idx,]

apart_rents = rbind(apart_rents[-idx, ], dups, use.names=TRUE)
message(sprintf("%.2f%% were duplicates.", 100-100*nrow(apart_rents)/na))
rm(idx, dups)


# how to combine houses and flats?
# assign a special value for vars that do not apply to one or another (flats/homes)
not4Homes = setdiff(names(apart_rents), names(home_rents))
not4Aparts = setdiff(names(home_rents), names(apart_rents))
special_code = 999L # by construction

home_rents[, (not4Homes) := special_code]
apart_rents[, (not4Aparts) := special_code]

rents = rbindlist(list(home_rents, apart_rents), use.names=TRUE)

if (!("grid_id" %in% names(rents))) {
  if ("ergg_1km" %in% names(rents)) {
    setnames(rents, "ergg_1km", "grid_id")
  } else if ("grid_id_char" %in% names(rents)) {
    setnames(rents, "grid_id_char", "grid_id")
  }
}

# reorder variables
order_of_vars = c(
  "uniqueid_gen", "obid", "kid2019", "zipcode", "grid_id", "year", "mon",
  "rent_cold", "utilities", "constr_year", "renov_year", "floor_space", "floor",
  "num_floors","num_rooms", "num_bedrooms", "num_bathrooms", "num_ancillary_rooms",
  "kitchen","condition", "balcony", "garden", "basement", "equipment", "heating_type",
  "type","house_type", "flat_type", "lab_mrkt_reg", "guest_washroom", "number_hits_of_ad"
)

setcolorder(rents, order_of_vars)
setnames(rents, "kid2019", "did")    # rename `kid2019` to `did` (district id)
setnames(rents, "rent_cold", "rent") # rename `rent_cold` to `rent`

rm(home_rents, apart_rents)

# cleaning -----
(missings = count_missing(rents) |> {\(x) x[order(-x$count), ]}())

## dealing with missing values of many forms -----------------------------------
n = nrow(rents)
rents = rents[grid_id > 0 & rent > 0 & floor_space > 0 & num_rooms > 0 & utilities > 0, ]
message(sprintf("%.2f%% observations dropped", 100-100*nrow(rents)/n))


### house type ----
# the combination of categories follows 'Klick & Schaffner' (2019)
rents[, house_type := fcase(
  house_type == -7 | house_type == -9,           0L,
  house_type == 1 | house_type == 2,             1L,
  house_type == 11 | house_type == 12,           2L,
  house_type == 3L,                              3L,
  between(house_type, 4, 6),                     4L,
  house_type == 13 | house_type == 15,           5L,
  between(house_type, 7, 10) | house_type == 14, 6L,
  house_type == special_code,                    special_code # 0-6 are for homes, `special_code` is for flats (WM_SUF)
                                                              # --a special type by construction
)]
rents[, house_type := factor(
  house_type, c(0L:6L, special_code),
  c(
    "na",            # -9 (Sonstiges Missing) + -7 (Keine Angabe)
    "single-family", #  1 Single-family house (detached) + 2 Single-family house
    "two-family",    # 11 two-family houses + 12 block of flats
    "semi-detached", # itself, 3 semi-detached
    "terraced",      # 4 terraced + 5 terraced (middle unit) + 6 terraced (end unit)
    "other",         # 13 other property for living + 15 other
    "special",       # 7 Bungalow + 8 Farmhouse + 9 Castle + 10 Mansion + 14 Special property
    "apartments"     # the above are house types, this is for flats (WM_SUF)
  )
)]

### apartment type ----
# category 11 has no label, this needs attention
rents[flat_type == 11 | flat_type == -7 | flat_type == -9, flat_type := 0L]
rents[, flat_type := factor(
  flat_type, c(0L:10L, special_code),
  c(
    "na",                  # -9 (Sonstiges Missing) + -7 (Keine Angabe)
    "attic",               #  1 Attic flat
    "ground-floor",        #  2 Ground floor flat
    "flat",                #  3 Flat
    "raised-ground-floor", #  4 Raised ground floor flat
    "loft",                #  5 Loft
    "Maisonette",          #  6 Maisonette
    "penthouse",           #  7 Penthouse
    "souterrain",          #  8 Souterrain
    "flat-with-terrace",   #  9 Flat with terrace
    "others",              # 10
    "houses"               # 0-10 are for flats, `special_code` is for houses (HM_SUF)--a special type by construction
  )
)]

# alternative to having two separate but related vars is combining the types into one var
rents[, htype := paste0(house_type, '-', flat_type)] # house and apartment types combined
rents[, htype := sub("^([a-z-]+?)-(houses)$", "\\2-\\1", htype)]
setcolorder(rents, 'htype', after = 'house_type')

### condition of the object ----
rents[condition < 0, condition := 0L]
rents[, condition := factor(
  condition,
  0L:10L,
  c(
    "na", "First occupancy", "First occupancy after reconstruction", "Like new",
    "Reconstructed", "Modernised", "Completely renovated", "Well kempt",
    "Needs renovation", "By arrangement", "Dilapidated"
  )
)]


### number of bedrooms ----
rents[, num_bedrooms := fcase(
  num_bedrooms <= 0, 0L,
  num_bedrooms >= 7, 7L,
  rep_len(TRUE, length(num_bedrooms)), num_bedrooms
)]
rents[, num_bedrooms := factor(
  num_bedrooms,
  0:7,
  c("na or 0", 1:6, "7+")
)]


### number of bathrooms ----
rents[, num_bathrooms := fcase(
  num_bathrooms <= 0, 0L,
  num_bathrooms >= 4, 4L,
  rep_len(TRUE, length(num_bathrooms)), num_bathrooms
)]
rents[, num_bathrooms := factor(
  num_bathrooms,
  0:4,
  c("na or 0", 1:3, "4+")
)]


### total number of floors, create 5 categories ----
rents[, num_floors := fcase(
  between(num_floors, -11, 0), 0L,
  between(num_floors, 4, max(num_floors)), 4L,
  rep_len(TRUE, length(num_floors)), num_floors
)]
rents[, num_floors := factor(num_floors, 0:4, c("na", 1:3, "4+"))]


### facilities of the house, create 5 categories ----
rents[between(equipment, -11, 0), equipment := 0]
rents[, equipment := factor(
  equipment,
  0:4,
  c("na", "Simple", "Normal", "Sophisticated", "Deluxe")
)]


### year of construction, and year of renovation ----
maxYear = max(rents$year)
rents[, c("constr_year_cat", "renov_year_cat") := lapply(.SD, function(x) {
  fcase(
    x <= 0, 0,
    x < 1900, 1,
    between(x, 1900, 1945), 2,
    between(x, 1946, 1959), 3,
    between(x, 1960, 1969), 4,
    between(x, 1970, 1979), 5,
    between(x, 1980, 1989), 6,
    between(x, 1990, 1999), 7,
    between(x, 2000, 2009), 8,
    between(x, 2010, maxYear), 9
  )
}), .SDcols = c("constr_year", "renov_year")]

rents[, c("constr_year_cat", "renov_year_cat") := lapply(.SD,
  factor,
  levels = 0:9,
  labels = c(
    "na", "<1900", "1900-1945", "1946-1959", "1960-1969", "1970-1979",
    "1980-1989", "1990-1999", "2000-2009", "2010+"
  )
), .SDcols = c("constr_year_cat", "renov_year_cat")]
rm(maxYear)


### Type of heating ----
rents[heating_type < 0, heating_type := 0]
# get_value_labels('heizungsart', TRUE) |> subset(value>=0, )
rents[, heating_type := factor(
  heating_type,
  0L:13L,
  c(
    "na", "Cogeneration/combined heat and power plant", "Electric heating",
    "Self-contained central heating", "District heating", "Floor heating",
    "Gas heating", "Wood pellet heating", "Night storage heaters", "Heating by stove",
    "Oil heating", "Solar heating", "Thermal heat pump", "Central heating"
  )
)]


### binary variables -----
# Following ('Klick & Schaffner' 2019, p. 12), for binary variables, we replace
# missing values by 0, i.e., by absence of the feature. Absence is denoted by `Nein` (== no ==0) in binary variables.

binary_vars = c(
  "basement", "protected_building", "guest_washroom", "holiday_house",
  "elevator", "balcony", "kitchen", "public_housing_cert", "betreut", "garden"
)
binary_vars = binary_vars[binary_vars %in% names(rents)] # keep if available in the data set

# replace '-9'--other missing or '-7'--not specified, by '0'.
for (i in seq_along(binary_vars)) {
  # check if absence of the info in a binary variable is denoted by 0
  if (all(0 %in% unique(rents[[binary_vars[[i]]]]))) {
    rents[, (binary_vars[[i]]) := lapply(.SD, function(v) {
      fcase(
        v == -9 | v == -7,           0L,
        between(v, -11, -1),        NA_integer_,
        rep_len(TRUE, length(v)),   v
      )
    }),
    .SDcols = binary_vars[[i]]
    ]
  } else {
    warning(sprintf(
      "Variable `%s` does not have `0` in its levels/categories.",
      binary_vars[[i]]
    ))
  }
}
rents[, (binary_vars) := lapply(.SD, as.factor), .SDcols = binary_vars]

# house keeping
rm(binary_vars, i)

### drop not-finished houses: House in process of planning or building ----
# rents = rents[!(constr_phase %like% "(House in process of )?(planning|building)"), ]


### drop districts that do not exist under the BKG (2019.12.31) definition, if any ----
districts = fread("data/geodata/admin-areas/districts.csv", select = "ags")
rents = merge(rents, districts, by.x = 'did', by.y = "ags")
rm(districts)


# compute distance to the CBD -----
# de_grid = st_read('data/geodata/germany-grid/de-grid.gpkg')
# lmrs = fread(
#   "data/geodata/labor-market-regions/Labor-Market-Regions_Kosfeld-Werner-2012_2019.csv",
#   select = c("amr_id", "district_id"),
#   col.names = c("amr_id", "did")
# )
# cbds = st_read('data/geodata/labor-market-regions/Labor-Market-Regions_Kosfeld-Werner-2012_2019.gpkg')[, c('amr_id', 'geom')] |> st_centroid(cbds)
# cbds = st_transform(cbds, st_crs(de_grid))
# cbds = merge(cbds, lmrs, 'amr_id')
#
# # geometry now is the centroid of the grid cell
# st_geometry(de_grid) = st_centroid(st_geometry(de_grid)) |> st_geometry()
# de_grid = merge(de_grid, unique(rents[, .(grid_id, did)]), by="grid_id")
#
# dids = as.integer(unique(cbds$did))
# dist2cbd = vector("list", length(dids))
# for (did in dids) {
#   grid_ids = which(as.integer(de_grid$did) == did)
#   dist2cbd[[did]] = data.frame(
#     grid_id = de_grid[grid_ids, ]$grid_id,
#     did = did,
#     dist2cbd = st_distance(de_grid[grid_ids, ], cbds[as.integer(cbds$did) == did, ])
#   )
# }
# dist2cbd = rbindlist(dist2cbd, use.names=TRUE)
# dist2cbd = merge(dist2cbd, lmrs, 'did')
# rents = merge(rents, dist2cbd, c('grid_id', 'did'))
# rm(lmrs, cbds,de_grid,dist2cbd)


## import consumer price index (CPI) for inflation adjustment ----
# source: https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61121&language=en#abreadcrumb
cpi = fread("data/cpi_61121-0002.csv", skip = 6, header = FALSE, select = 1:3,
  na.strings = "...", col.names = c("year", "mon", "cpi")
)
cpi[, year := as.integer(year)
    ][, mon := match(mon, month.name)]

## adjust by the GDP deflator (CPI)
rents = merge(rents, cpi[year >= min(rents$year), ],
  by.x = c("year", "mon"), by.y = c("year", "mon")
)
rents[, rent := rent / (cpi / 100)]  # divide by the deflator
rents[, utilities := utilities / (cpi/100)]
rents[, cpi := NULL] # remove cpi column


# zipcodes -----
# rents = rents[!(zipcode %like% "^-(000)?"), ] # rm missing zip codes, if any
rents[, `:=`(zipcode = sprintf("%05i", zipcode))] # make 5 digit


# Define new vars ----
rents[, c("lnrent", "rent_sqm") := .(log(rent), rent / floor_space)]
rents[, lnrent_sqm := log(rent_sqm)]
rents[, lnutilities := log(utilities)]
<<<<<<< HEAD
setkeyv(rents, c("did", "zipcode", "ad_end_mon", "year"))
=======
setkeyv(rents, c("did", "zipcode", "mon", "year"))

# optional: further cleaning for  hedonic model-----

# problematic construction and renovation years
rents[constr_year < 0, constr_year := NA][renov_year < 0, renov_year := NA]

# perhaps houses not finished or built yet
maxYear = rents[, max(year)] # min(2021, rents[, max(year)])
rents[constr_year > maxYear, constr_year := (maxYear)]
# If renovated before built, swap construction year with renovation year
rents[renov_year < constr_year,`:=`(renov_year = constr_year, constr_year = renov_year)]

# keep houses built since 1900
rents = rents[constr_year >= 1900 | is.na(constr_year), ]

# imputation of NAs with the overall median value by house type

# fill missing construction year by renovation year:
# could be that not renovated yet, thus construction year == renovation year
rents[is.na(constr_year) & !is.na(renov_year), constr_year := renov_year]
rents[is.na(renov_year) & !is.na(constr_year), renov_year := constr_year]

rents[, constr_year := fifelse(
  is.na(constr_year), floor(median(constr_year, na.rm = TRUE)), constr_year
  ), house_type]

# impute renovation year by the median value,
# if not, replace it with construction year
rents[, renov_year := fifelse(
  is.na(renov_year), max(constr_year, floor(median(renov_year, na.rm = TRUE))), renov_year
  ), house_type]

# sanity checks
if (any(idx <- rents[, constr_year < 1900])) {
  message(sprintf("The imputation produced %i very old houses: built before 1900. Removing them...", sum(idx)))
  rents = rents[!idx, ]
}

if (any(rents[, renov_year > maxYear])) {
  message("The imputation produced for some homes renov.year > max.year possible. Replaced them by the max.year")
  rents[renov_year > maxYear, renov_year := (maxYear)]
}

# compute age of houses, and time passed since last renovation
rents[, age0 := year - constr_year] # age of the property at the time of advertisement
rents[, age1 := year - renov_year]


# handle outliers ----
## discard properties with:
# (i) a monthly rental price below 1euro/m2 or above 50euro/m2
# (ii) floor space below 30m2 or above 500m2
n = nrow(rents)
rents = rents[
  exp(lnrent_sqm) >= 1 & exp(lnrent_sqm) <= 50 & floor_space >= 30 & floor_space <= 500,
  ]
nrow(rents)/n

# write to disk ----
fpath = "data/processed/rents_clean.csv" # path for saving the file
if (file.exists(fpath)) {
  warning("File has been overwritten!", call. = FALSE)
}
fwrite(rents, fpath)
