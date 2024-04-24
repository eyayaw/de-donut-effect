library(data.table)

municipals = fread(
  "data/geodata/admin-areas//municipalities_bkg.csv",
  select = c("ags", "geo_name", "district_ags"), keepLeadingZeros = TRUE
)
setnames(municipals, "district_ags", "did")

# population at the municipality level, includes for higher levels too
pop = fread(
  "data/population/12411-01-01-5_flat.csv",
  select = c(
    "Zeit", "1_Auspraegung_Code", "1_Auspraegung_Label",
    "2_Auspraegung_Label", "BEVSTD__Bevoelkerungsstand__Anzahl"
  ),
  na.strings = c(".", "/", "...", "-"),
  encoding = "Latin-1"
)

setnames(pop, c("year", "ags", "geo_name", "gender", "pop_level"))
pop[, year := as.integer(sub("\\d{2}\\.\\d{2}\\.(\\d{4})", "\\1", year))]
setcolorder(pop, c("ags", "geo_name", "year"))
setkey(pop, ags, year)

# reshape pop_level to wide
pop = dcast(pop, ... ~ gender, value.var = "pop_level")
setnames(
  pop,
  c("Insgesamt", "männlich", "weiblich"),
  sprintf("pop_%s", c("tot", "male", "female"))
)


higher_levels = pop[
  ags == "DG" | # Germany
    ags %in% sprintf("%02d", 1:16) | # 16 states
    nchar(ags) == 5L, # districts
  which = TRUE
]
aggregates = pop[higher_levels, ] # values for the states and country Germany
pop = pop[-higher_levels, ] # keep only lower levels

# municipality population
pop_mun = merge(municipals, pop[, !"geo_name"], "ags")

# these are possibly district-municipalities, one municipality in the district
distr_municipals = municipals[!pop, on = "ags"] # anti-join, -> not matching                                                        # -> district-municipalities
## we can check with
# distr_mun = municipals[municipals[, .N, did][N==1L, !'N'], on='did']
# distr_municipals[distr_mun, on=c('ags', 'did', 'geo_name')]

# add population data
distr_municipals = distr_municipals[aggregates[, !"geo_name"], on = "did==ags", nomatch = NULL]

## city-states with only 1 municipality, Berlin and Hamburg
## Bremen is ok!
city_states = municipals[ags %like% "^(02|11)" & geo_name %like% "Berlin|Hamburg", ]
city_states[, state_code := sub("^(02|11)\\d+$", "\\1", ags)]
city_states = city_states[aggregates[ags %like% "^(02|11)" & geo_name %like% "Berlin|Hamburg", !"geo_name"], on = "state_code==ags"][, !"state_code"]


# all municipalities
pop_mun = list(pop_mun, distr_municipals, city_states) |>
  rbindlist(use.names = TRUE)

setkey(pop_mun, ags, year)


# fill na values with the mean value
cols = sprintf("pop_%s", c("tot", "male", "female"))
setkey(pop_mun, ags, year)
pop_mun[, (cols) :=
  lapply(.SD, \(x) nafill(x, type = "const", fill = round(mean(x, na.rm = TRUE)))),
.SDcols = cols, ags
]

pop_mun = pop_mun[!is.na(pop_tot), ] # uninhabited

# create ranking by population by year ----
pop_mun[, rank_pop := frank(-pop_tot, ties.method = "dense"), year]
setnames(pop_mun, "pop_tot", "pop_mun")

# write to disk
pop_mun |> fwrite("data/processed/population_municipality.csv")

## extra ----
# former municipalities but later merged or dissolved?
old_municipals = pop[!municipals, on = "ags", .N, .(ags, geo_name)]
