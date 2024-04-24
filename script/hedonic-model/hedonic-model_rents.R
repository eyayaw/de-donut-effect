library(fixest)
library(data.table)
source("script/helpers/helpers.R")

rents = fread("data/processed/rents_clean.csv", keepLeadingZeros = TRUE)
dists = fread(
  "./data/geodata/grids_dist2cbd.csv",
  select = c("grid_id", "dist_inhab"),
  keepLeadingZeros = TRUE
)

# cleaning ----
rents = rents[zipcode > 0, ]
rents[, unique_plz := paste0(sprintf("%05i", as.integer(zipcode)), "-", sprintf("%08i", mid_ags2019))]

rents = merge(rents, dists[, .(grid_id, dist = dist_inhab)])
rents[, lndist := log(1 + 1e-3 * dist)]


## required variables
dep_var = "lnrent_sqm"
fixeffs = c("unique_plz", "year", "mon")
num_vars = c("floor_space", "num_rooms", "age0", "age1", "lnutilities", "lndist")
### factor variables
cats = c(
  fixeffs, "num_bedrooms", "num_bathrooms", "num_floors", "house_type", "flat_type",
  "heating_type", "basement", "equipment", "condition", "balcony", "garden",
  "kitchen", "floor", "guest_washroom"
)

var_list = c(fixeffs, dep_var, num_vars, setdiff(cats, fixeffs))
rents = rents[, c(var_list, "type"), with = FALSE]

# construct factors
rents[, (cats) := lapply(.SD, as.factor), .SDcols = c(cats)]

## run separate hedonics for homes and apartments
homes = rents[type == 2, ][, type := NULL]  #  2 Haus-Miete
aparts = rents[type == 4, ][, type := NULL] #  4 Wohnung-Miete

not4Homes = c(
  "floor", "wohngeld", "elevator", "balcony", "betreut", "kitchen",
  "public_housing_cert", "garden", "flat_type"
)
not4Aparts = c("plot_size", "granny_flat", "constr_phase", "house_type")

homes[, intersect(not4Homes, names(homes)) := NULL]
aparts[, intersect(not4Aparts, names(aparts)) := NULL]

rm(rents)
gc()


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
gc()

hedonic_rentals = lapply(list(aparts = aparts, homes = homes), \(x) {
  mod = feols(makeFormula(x), x, combine.quick = FALSE, mem.clean = TRUE)
  invisible(gc())
  fe = tidy_fixeff(fixef(mod)) |> as.data.table() # extract fixed effects
  list(mod = mod, fe = fe)
})


# write to disk ----
fwrite(hedonic_rentals$homes$fe, "data/processed/HRI_fixed-effects_homes.csv")
fwrite(hedonic_rentals$aparts$fe, "data/processed/HRI_fixed-effects_aparts.csv")

# export regression output ----
etable(lapply(hedonic_rentals, `[[`, "mod"),
  depvar = FALSE,
  headers = "All homes",
  tex = TRUE,
  title = "Hedonic Rental Index",
  label = "hedonic",
  file = "data/processed/hedonic-output_rents.tex",
  style.df = style.df("aer"),
  replace = TRUE
)
