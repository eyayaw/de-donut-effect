library(fixest)
library(data.table)
source("./script/helpers/helpers.R")

# import data ----
purchases = fread("./data/processed/purchases_clean.csv", keepLeadingZeros = TRUE)
dists = fread(
  "./data/geodata/grids_dist2cbd.csv",
  select = c("grid_id", "dist_inhab"),
  keepLeadingZeros = TRUE
)

# cleaning ----
purchases = purchases[zipcode > 0, ]
purchases[, unique_plz := paste0(sprintf("%05i", as.integer(zipcode)), "-", sprintf("%08i", as.integer(mid_ags2019)))]

purchases = merge(purchases, dists[, .(grid_id, dist = dist_inhab)])
purchases[, lndist := log(1 + 1e-3 * dist)]

# required variables
dep_var = "lnprice_sqm"
fixeffs = c("unique_plz", "year", "mon")
num_vars = c("floor_space", "num_rooms", "age0", "age1", "lndist")
# factor variables
cats = c(
  fixeffs, "num_bedrooms", "num_bathrooms", "num_floors", "house_type", "flat_type",
  "holiday_house", "heating_type", "basement", "guest_washroom", "constr_phase",
  "equipment", "condition", "balcony", "garden", "kitchen", "floor"
)

var_list = c(fixeffs, dep_var, num_vars, setdiff(cats, fixeffs))
purchases = purchases[, c(var_list, "type"), with = FALSE]

# construct factors
purchases[, (cats) := lapply(.SD, as.factor), .SDcols = c(cats)]

# run separate hedonics for homes and apartments ----
homes = purchases[type == 1, ][, type := NULL]  #  1 Haus-Kauf
aparts = purchases[type == 3, ][, type := NULL] #  3 Wohnung-Kauf

not4Homes = c(
  "floor", "wohngeld", "elevator", "balcony", "betreut", "kitchen",
  "public_housing_cert", "garden", "flat_type"
)
not4Aparts = c("plot_size", "granny_flat", "constr_phase", "house_type")

homes[, intersect(not4Homes, names(homes)) := NULL]
aparts[, intersect(not4Aparts, names(aparts)) := NULL]

rm(purchases)
invisible(gc())

# estimation ----
makeFormula = function(.data, depvar = dep_var, env = parent.frame()) {
  avail = names(.data)
  rhs = c(num_vars, setdiff(cats, fixeffs)) |>
    intersect(avail) |>
    paste(collapse = " + ")
  f = sprintf("%s ~ %s | %s",
    depvar, rhs, paste(intersect(fixeffs, avail), collapse = "^")
  )
  as.formula(f, env = env)
}

hedonic_purchases = list(aparts = aparts, homes = homes) |>
  lapply(\(x) {
    mod = feols(makeFormula(x), x, combine.quick = FALSE, mem.clean = TRUE)
    fe = tidy_fixeff(fixef(mod)) |> as.data.table() # extract fixed effects
    list(mod = mod, fe = fe)
  })



# write to disk ----

fwrite(hedonic_purchases$homes$fe, "data/processed/HPI_fixed-effects_homes.csv")
fwrite(hedonic_purchases$aparts$fe, "data/processed/HPI_fixed-effects_aparts.csv")

# export regression output ----
etable(lapply(hedonic_purchases, `[[`, "mod"),
  depvar = FALSE,
  headers = "All homes",
  tex = TRUE,
  title = "Hedonic Price Index",
  label = "hedonic",
  file = "data/processed/hedonic-output_prices.tex",
  style.df = style.df("aer"),
  replace = TRUE
)
