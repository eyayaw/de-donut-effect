library(fixest)
library(data.table)
source("script/helpers/helpers.R")


rents = fread("data/processed/rents_clean.csv")
rents[, year_mon := paste0(year, "-", mon)]
## required variables
dep_var = "lnrent_sqm"
fixeffs = "year_mon" # c("year", "mon")
num_vars = c("floor_space", "num_rooms", "age0", "age1", "lnutilities")
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
get_doesnot_apply_vars <- function(data) {
  names(data)[vapply(data, \(x) length(unique(sample(x, 25))) == 1, NA)]
}


# Estimation -------------------------------------------------------------------
makeFormula = function(.data, depvar = dep_var, env = parent.frame()) {
  avail = names(.data)
  rhs = c(num_vars, setdiff(cats, fixeffs)) |>
    intersect(avail) |>
    paste(collapse = " + ")
  f = sprintf(
    "%s ~ 0 + %s | %s",
    depvar, rhs, paste(intersect(fixeffs, avail), collapse = "^")
  )
  as.formula(f, env = env)
}


# iteration
property_types = list(
  homes = 2, #  2 Haus-Miete
  aparts = 4 #  4 Wohnung-Miete
)


for (i in seq_along(property_types)) {
  mod = rents[type == property_types[[i]], ] |>
    {
      \(d) d[, !get_doesnot_apply_vars(d), with = FALSE]
    }() |>
    {
      \(.d) feols(makeFormula(.d), .d, combine.quick = FALSE, nthreads = 0.8, mem.clean = TRUE)
    }()

  invisible(gc())

  fe = tidy_fixeff(fixef(mod)) |> as.data.table() # extract fixed effects

  fwrite(
    fe,
    sprintf("data/processed/HRI_fixed-effects_%s_yearly.csv", names(property_types)[[i]])
  )
  rm(mod)
  gc()
}
