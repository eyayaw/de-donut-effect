# This file estimates the donut effect for CBD and suburbs.

#--------------------------------------------------
# paths

main_path <- getwd()
data_path <- file.path(main_path, "data")
output_path <- file.path(main_path, "output")

#--------------------------------------------------
# libraries

library(data.table)
library(dplyr)
library(fixest)
library(stringr)
library(ggplot2)

#--------------------------------------------------
# load data

housing_amenity <- data.table::fread(
  file.path(
      data_path,
      "processed/housing_amenities.csv"
  )
)

# zipcode data
zipcodes <- data.table::fread(
  file.path(
      data_path,
      "geodata/zip-codes/zip-codes_dist2cbd.csv"
  )
)

#--------------------------------------------------
# define quantile distances
# for determining CBD rings, and suburb area
# rule:
# 0-10%: CBD
# 10-30%: suburb ring 1
# 30-50%: suburb ring 2
# > 50%: outside

dist_quants <- as.numeric(
    quantile(
        zipcodes$dist_inhab,
        prob = seq(0, 1, 0.1),
        na.rm = TRUE
    )
)

# classify zipcodes
zipcodes <- zipcodes |>
    mutate(
        cbd_suburb = case_when(
            dist_inhab <= dist_quants[2] ~ "cbd",
            dist_inhab > dist_quants[2] & dist_inhab <= dist_quants[4] ~ "suburb_ring_1",
            dist_inhab > dist_quants[4] & dist_inhab <= dist_quants[6] ~ "suburb_ring_2",
            TRUE ~ "outside"
        )
    )

# merge to amenity data
housing_amenity <- merge(
    housing_amenity,
    zipcodes |> select(unique_plz, cbd_suburb),
    by = "unique_plz",
    all.x = TRUE
)

#--------------------------------------------------
# add logarithmic information

housing_amenity <- housing_amenity |>
    mutate(
        lndist = log(1 + dist_inhab / 1000),
        lnpop_dens = log(pop_mun / area_qkm),
        lnpop = log(1 + inhabitants),
        lnpp = log(1 + tot_purchase_power),
    )

#--------------------------------------------------
# add pandemic dummy

housing_amenity <- housing_amenity |>
    mutate(
        pandemic = case_when(
            time >= "2020-03-01" ~ 1,
            TRUE ~ 0
        )
    )

#--------------------------------------------------
# define estimation function

est_function <- function(data, hedonic, spec = c("no_controls", "controls")) {
    # subset data
    dta <- data |>
        filter(var == hedonic) |>
        # data restricted to 2017 and onwards because amenity data does start
        # only in 2017
        filter(year >= "2017-01-01") |>
        # add month-year variable
        mutate(
            year_mon = as.factor(
                substr(time, start = 1, stop = 7)
            )
        )

    # define controls
    controls <- c("lnpp + share_male_1845 + share_background_german")

    # define formula
    if (spec == "no_controls") {
        fml <- formula(
            paste0(
                "val ~ year_mon | amr_name"
            )
        )
    } else {
        fml <- formula(
            paste0(
                "val ~ year_mon +", controls, "| amr_name"
            )
        )
    }

    # estimate model
    est_mod <- feols(
        fml = fml,
        data = dta,
        se = "hetero"
    )

    # return output
    return(est_mod)
}

#--------------------------------------------------
# estimation

# define hedonic types
hedonic_types <- unique(housing_amenity$var)

# define specification
species <- c("no_controls", "controls")

# cbd vs suburb
areas <- c("cbd", "suburb_ring_1", "suburb_ring_2")

# estimation findings
est_results_list <- list()

for(hedonic_type in hedonic_types) {
    for(area in areas) {
        for(speci in species) {
            # subset data
            dta <- housing_amenity |>
                filter(cbd_suburb == area)

            # estimate model given inputs
            mod <- est_function(
                data = dta,
                hedonic = hedonic_type,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(area),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

#--------------------------------------------------
# get coefficients

get_cofficients <- function(result) {
    # subset data list
    est_data <- est_results_list[[result]]

    # extract coefficients
    coef <- as.data.frame(est_data$coefficients)
    coef$var <- row.names(coef)
    row.names(coef) <- seq(1, nrow(coef), 1)

    # cleaning
    coef_prep <- coef |>
        rename(
            coefficient = `est_data$coefficients`
        ) |>
        # keep only month coefficients
        filter(
            stringr::str_detect(var, "year_mon")
        ) |>
        mutate(
            year_mon = substr(
                var,
                start = nchar(var) - 6,
                stop = nchar(var)
            )
        ) |>
        # drop variable names
        select(-var)

    # calculate difference to March 2020
    # add hedonic type, area (cbd vs suburb) and specification
    base_value <- coef_prep$coefficient[coef_prep$year_mon == "2020-03"]
    coef_prep <- coef_prep |>
        mutate(
            diff_mar_2020 = (coefficient / base_value) * 100,
            hedonic_type = str_split_fixed(result, "-", n = 3)[1],
            area = str_split_fixed(result, "-", n = 3)[2],
            specification = str_split_fixed(result, "-", n = 3)[3]
        )

    # return
    return(coef_prep)
}

# empty list for storage
coef_list <- list()

# loop through results
for(r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# row bind all data
coefs <- rbindlist(coef_list)

# add rolling average (3-months interval)
coefs <- coefs |>
    group_by(hedonic_type, area, specification) |>
    mutate(diff_mar_2020_rollmean = frollmean(diff_mar_2020, na.rm = TRUE, n = 3))

#--------------------------------------------------
# plotting

# define colors
cols <- c("#2b8043", "#1746A2", "#c70002")

# plot function
plot_function <- function(hedonic, spec, naming) {
    # subset data
    dta <- coefs |>
        filter(hedonic_type == hedonic) |>
        filter(specification == spec) |>
        # add plot date
        mutate(
            plot_date = as.Date(paste0(year_mon, "-01"))
        )

    # generate plot
    plot <- ggplot(data = dta)+
        geom_line(
            mapping = aes(
                x = plot_date,
                y = diff_mar_2020_rollmean,
                group = area,
                col = area
            ),
            linewidth = 1
        )+
        scale_color_manual(
            values = c(
                "CBD" = cols[3],
                "SUBURB_RING_1" = cols[1],
                "SUBURB_RING_2" = cols[2]
            ),
            labels = c(
                "CBD" = "CBD",
                "SUBURB_RING_1" = expression(Suburb["Ring 1"]),
                "SUBURB_RING_2" = expression(Suburb["Ring 2"])
            ),
            name = ""
        )+
        geom_vline(
            xintercept = as.Date("2020-03-01"),
            linewidth = 0.8,
            linetype = "dashed",
            col = "black"
        )+
        labs(
            x = "",
            y = "Hedonic index (March 2020 = 100)"
        )+
        theme_light()+
        theme(
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            legend.position = "bottom",
            legend.key.size = unit(0.6, "cm"),
            legend.text = element_text(size = 20)
        )

    # export
    ggsave(
        plot = plot,
        file.path(
            output_path,
            "tabs",
            paste0(naming, "_donut_", hedonic, "_", spec, ".png")
        )
    )
}

# define options
hedonic_types <- unique(coefs$hedonic_type)
specis <- unique(coefs$specification)

# loop through options
for (type in hedonic_types) {
    for (speci in specis) {
        plot_function(
            hedonic = type,
            spec = speci,
            naming = "all"
        )
    }
}

#--------------------------------------------------
# donut effect for 15 largest cities

# filter for largest cities
# population at least 500,000 + Duisburg which is slightly below
# gives 15 largest cities
largest_cities <- housing_amenity |>
    filter(pop_mun >= 500000 | geo_name == "Duisburg")

# run estimation again
est_results_list <- list()
hedonic_types <- unique(housing_amenity$var)

for(hedonic_type in hedonic_types) {
    for(area in areas) {
        for(speci in species) {
            # subset data
            dta <- largest_cities |>
                filter(cbd_suburb == area)

            # estimate model given inputs
            mod <- est_function(
                data = dta,
                hedonic = hedonic_type,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(area),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

# get coefficients
coef_list <- list()

for(r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# row bind all data
coefs <- rbindlist(coef_list)

# add rolling average (3-months interval)
coefs <- coefs |>
    group_by(hedonic_type, area, specification) |>
    mutate(diff_mar_2020_rollmean = frollmean(diff_mar_2020, na.rm = TRUE, n = 3))

# generate plots
hedonic_types <- unique(coefs$hedonic_type)
specis <- unique(coefs$specification)

for (type in hedonic_types) {
    for (speci in specis) {
        plot_function(
            hedonic = type,
            spec = speci,
            naming = "largest_cities"
        )
    }
}

#--------------------------------------------------
# donut effect for cities with at least 100,000 population
# but smaller than 500,000

# filter cities
medium_cities <- housing_amenity |>
    filter(pop_mun >= 100000 & geo_name != "Duisburg") |>
    filter(pop_mun < 500000)

# run estimation again
est_results_list <- list()
hedonic_types <- unique(housing_amenity$var)

for(hedonic_type in hedonic_types) {
    for(area in areas) {
        for(speci in species) {
            # subset data
            dta <- medium_cities |>
                filter(cbd_suburb == area)

            # estimate model given inputs
            mod <- est_function(
                data = dta,
                hedonic = hedonic_type,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(area),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

# get coefficients
coef_list <- list()

for(r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# row bind all data
coefs <- rbindlist(coef_list)

# add rolling average (3-months interval)
coefs <- coefs |>
    group_by(hedonic_type, area, specification) |>
    mutate(diff_mar_2020_rollmean = frollmean(diff_mar_2020, na.rm = TRUE, n = 3))

# generate plots
hedonic_types <- unique(coefs$hedonic_type)
specis <- unique(coefs$specification)

for (type in hedonic_types) {
    for (speci in specis) {
        plot_function(
            hedonic = type,
            spec = speci,
            naming = "medium_cities"
        )
    }
}

#--------------------------------------------------
# donut effect for cities below 100,000 population

# filter cities
small_cities <- housing_amenity |>
    filter(pop_mun < 100000)

# run estimation again
est_results_list <- list()
hedonic_types <- unique(housing_amenity$var)

for(hedonic_type in hedonic_types) {
    for(area in areas) {
        for(speci in species) {
            # subset data
            dta <- small_cities |>
                filter(cbd_suburb == area)

            # estimate model given inputs
            mod <- est_function(
                data = dta,
                hedonic = hedonic_type,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(area),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

# get coefficients
coef_list <- list()

for(r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# row bind all data
coefs <- rbindlist(coef_list)

# add rolling average (3-months interval)
coefs <- coefs |>
    group_by(hedonic_type, area, specification) |>
    mutate(diff_mar_2020_rollmean = frollmean(diff_mar_2020, na.rm = TRUE, n = 3))

# generate plots
hedonic_types <- unique(coefs$hedonic_type)
specis <- unique(coefs$specification)

for (type in hedonic_types) {
    for (speci in specis) {
        plot_function(
            hedonic = type,
            spec = speci,
            naming = "small_cities"
        )
    }
}
