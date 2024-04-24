# This file estimates the impact of amenities on housing values.

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

est_function <- function(data, hedonic, index, spec = c("no_controls", "controls")) {
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
                "val ~", index_var, "* relevel(year_mon, \"2020-03\") | amr_name + year_mon"
            )
        )
    } else {
        fml <- formula(
            paste0(
                "val ~", index_var, "* relevel(year_mon, \"2020-03\") +", controls, "| amr_name + year_mon"
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

# define index variables
index_vars <- housing_amenity |>
    select(urbam_index, environ_area_index) |>
    names()

# define hedonic types
hedonic_types <- unique(housing_amenity$var)

# define specification
species <- c("no_controls", "controls")

# estimation findings
est_results_list <- list()

for(hedonic_type in hedonic_types) {
    for(index_var in index_vars) {
        for(speci in species) {
            # estimate model given inputs
            mod <- est_function(
                data = housing_amenity,
                hedonic = hedonic_type,
                index = index_var,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(index_var),
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
    coef <- as.data.frame(
        cbind(
            summary(est_data)$coefficients,
            summary(est_data)$se
        )
    )
    coef$var <- row.names(coef)
    row.names(coef) <- seq(1, nrow(coef), 1)

    confint_level <- 1.96

    # cleaning
    coef_prep <- coef |>
        rename(
            coefficient = V1,
            se = V2
        ) |>
        # keep only interactions
        filter(
            stringr::str_detect(var, ":")
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

    # add rolling average (3-months interval)
    coef_prep <- coef_prep |>
        mutate(
            coefficient_rollmean = frollmean(coefficient, na.rm = TRUE, n = 3),
            lower = coefficient_rollmean - (confint_level * se),
            upper = coefficient_rollmean + (confint_level * se)
        ) |>
        select(-se)

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

#--------------------------------------------------
# plot estimations

# define colors
cols <- c("#2b8043", "#1746A2", "#c70002")

# plotting function
plot_function <- function(result, naming = c("all", "cbd", "suburb1", "suburb2")) {
    # subset data
    dta <- coef_list[[result]] |>
        # add reference point
        add_row(
            year_mon = "2020-03",
            coefficient_rollmean = 0,
            lower = NA,
            upper = NA
        ) |>
        # add plot date
        mutate(
            plot_date = as.Date(paste0(year_mon, "-01"))
        )

    # define colors for consumption and environmental amenities
    if (str_detect(result, "URBAM")) {
        coloring = cols[2]
    } else {
        coloring = cols[1]
    }

    # generate plot
    plot <- ggplot()+
    geom_pointrange(
        data = dta,
        mapping = aes(
            x = as.Date(plot_date, "%Y-%m-%d"),
            y = coefficient_rollmean,
            ymin = lower,
            ymax = upper
        ),
        col = coloring,
        linewidth = 1,
        size = 0.5
    )+
    geom_vline(
        xintercept = as.Date("2020-03-01", "%Y-%m-%d"),
        linewidth = 0.8,
        linetype = "dashed",
        col = "black"
    )+
    geom_line(
        data = dta,
        mapping = aes(
            x = as.Date(plot_date, "%Y-%m-%d"),
            y = coefficient_rollmean
        ),
        col = "black",
        linewidth = 0.6
    )+
    labs(
        x = "",
        y = "Coefficients and 95% CI"
    )+
    theme_light()+
    theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)
    )

    # export
    ggsave(
        plot = plot,
        file.path(
            output_path,
            "tabs",
            paste0("est_", naming, "_", result, ".png")
        )
    )
}

# loop through all estimations
for (r in names(coef_list)) {
    plot_function(result = r, naming = "all")
}

#--------------------------------------------------
# estimation for CBDs

# subset for CBDs
cbds <- housing_amenity |>
    filter(cbd_suburb == "cbd")

# estimation
est_results_list <- list()

for(hedonic_type in hedonic_types) {
    for(index_var in index_vars) {
        for(speci in species) {
            # estimate model given inputs
            mod <- est_function(
                data = cbds,
                hedonic = hedonic_type,
                index = index_var,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(index_var),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

# get coefficients
coef_list <- list()

for (r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# generate plots
for (r in names(coef_list)) {
    plot_function(result = r, naming = "cbd")
}

#--------------------------------------------------
# estimation for suburb ring 1

# subset for suburb ring 1
suburb1 <- housing_amenity |>
    filter(cbd_suburb == "suburb_ring_1")

# estimation
est_results_list <- list()

for(hedonic_type in hedonic_types) {
    for(index_var in index_vars) {
        for(speci in species) {
            # estimate model given inputs
            mod <- est_function(
                data = suburb1,
                hedonic = hedonic_type,
                index = index_var,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(index_var),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

# get coefficients
coef_list <- list()

for (r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# generate plots
for (r in names(coef_list)) {
    plot_function(result = r, naming = "suburb_1")
}

#--------------------------------------------------
# estimation for suburb ring 2

# subset for suburb ring 2
suburb2 <- housing_amenity |>
    filter(cbd_suburb == "suburb_ring_2")

# estimation
est_results_list <- list()

for(hedonic_type in hedonic_types) {
    for(index_var in index_vars) {
        for(speci in species) {
            # estimate model given inputs
            mod <- est_function(
                data = suburb2,
                hedonic = hedonic_type,
                index = index_var,
                spec = speci
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(index_var),
                toupper(speci),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }
}

# get coefficients
coef_list <- list()

for (r in names(est_results_list)) {
    coefs <- get_cofficients(r)
    coef_list[[r]] <- coefs
}

# generate plots
for (r in names(coef_list)) {
    plot_function(result = r, naming = "suburb_2")
}
