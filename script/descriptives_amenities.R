# This file generates descriptives for amenities.

#--------------------------------------------------
# paths

main_path <- getwd()
data_path <- file.path(main_path, "data")
output_path <- file.path(main_path, "output")

#--------------------------------------------------
# libraries

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(fixest)

#--------------------------------------------------
# load data

# zipcode data
zipcodes <- data.table::fread(
    file.path(
        data_path,
        "geodata/zip-codes/zip-codes_dist2cbd.csv"
    )
)

# normalized amenity data and housing data combined
housing_amenity <- data.table::fread(
    file.path(
        data_path,
        "processed/housing_amenities.csv"
    )
)

# raw environmental data
environ_amen <- st_read(
    file.path(
        data_path,
        "geodata/amenities/water_green_combined_germany.gpkg"
    ),
    quiet = TRUE
) |>
    st_drop_geometry()

# raw amenity data
cons_amen <- st_read(
    file.path(
        data_path,
        "geodata/amenities/amenities_combined_germany.gpkg"
    ),
    quiet = TRUE
) |>
    st_drop_geometry()

# environmental amenity index
environ_index <- fread(
    file.path(
        data_path,
        "processed/environ_amenities_normalized_wide.csv"
    )
)

# consumption amenity index
cons_index <- fread(
    file.path(
        data_path,
        "processed/cons_amenities_normalized_wide.csv"
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

# export thresholds
write.table(
    dist_quants,
    file.path(
        output_path,
        "descriptives/percentiles_distances_zipcode.txt"
    ),
    col.names = FALSE
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
# summarise raw data per zipcode

summarise_by_zipcode <- function(amen_data, cons = FALSE) {
    # if consumption amenities summarise also by category
    if (cons == TRUE) {
        counts <- amen_data |>
            group_by(unique_plz, year, amenity_cat) |>
            summarise(
                N = n()
            )
    } else {
        # extract area covered variable
        covered_var <- amen_data |>
            select(contains("covered")) |>
            names()

        # summarise number of obs by zipcode
        counts <- amen_data |>
            group_by(unique_plz, year) |>
            summarise(
                N = n(),
                N_by_people = N / sum(einwohner, na.rm = TRUE),
                avg_area_covered = mean(.data[[covered_var]])
            )
    }

    # add additional zipcode information
    counts <- merge(
        counts,
        zipcodes,
        by = c("unique_plz"),
        all.x = TRUE
    )

    # return output
    return(counts)
}

# apply function
environ_counts <- summarise_by_zipcode(environ_amen, cons = FALSE)
cons_counts <- summarise_by_zipcode(cons_amen, cons = TRUE)

#--------------------------------------------------
# plot raw data counts with respect to distance to CBD

# define colors
cols <- c("#2b8043", "#1746A2", "#c70002")

count_data <- list(environ_counts, cons_counts)
count_data_names <- c("environ", "consump")

i <- 1
for (dta in count_data) {
    # subset data
    if (count_data_names[i] != "consump") {
        # for environmental amenities
        # limit frequency to remove outliers
        dta_subset <- dta |>
            filter(dist_inhab <= 25000) |>
            filter(N <= 2000)

        # limit area covered to remove outliers
        dta_subset_area <- dta |>
            filter(dist_inhab <= 25000) |>
            filter(avg_area_covered <= 30)
    } else {
        dta_subset <- dta |>
            filter(dist_inhab <= 25000)
    }

    # plot frequencies
    plot <- ggplot(
        data = dta_subset,
        aes(x = dist_inhab / 1000, y = N)
    ) +
        geom_point() +
        geom_smooth(method = "lm", col = "blue", linewidth = 1.25) +
        labs(
            x = "Distance to the CBD (km)",
            y = "Frequency"
        ) +
        theme_light() +
        theme(
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 18)
        )

    ggsave(
        plot = plot,
        file.path(
            output_path,
            "figs",
            paste0("frequency_CBD_distance_", count_data_names[i], ".png")
        )
    )

    # plot area covered for environmental amenities
    if (count_data_names[i] != "consump") {
        plot <- ggplot(
            data = dta_subset_area,
            aes(x = dist_inhab / 1000, y = avg_area_covered)
        ) +
            geom_point() +
            geom_smooth(method = "lm", col = "blue", linewidth = 1.25) +
            labs(
                x = "Distance to the CBD (km)",
                y = "Avg. area covered (sq. km)"
            ) +
            theme_light() +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 18)
            )

        ggsave(
            plot = plot,
            file.path(
                output_path,
                "figs",
                paste0("area_CBD_distance_", count_data_names[i], ".png")
            ),
            dpi = 300
        )
    }

    # update counter
    i <- i + 1
}

#--------------------------------------------------
# descriptives for counts

# generate descriptives
environ_counts |>
    group_by(cbd_suburb) |>
    summarise(
        across(
            .cols = c(N, N_by_people, avg_area_covered),
            ~ mean(.x, na.rm = TRUE)
        )
    ) |>
    # remove NA row
    filter(!is.na(cbd_suburb)) |>
    # export
    openxlsx::write.xlsx(
        file.path(
            output_path,
            "descriptives/avg_counts_environmental_amenities.xlsx"
        )
    )

cons_counts |>
    group_by(cbd_suburb) |>
    summarise(
        across(
            .cols = N,
            ~ mean(.x, na.rm = TRUE)
        )
    ) |>
    # remove NA row
    filter(!is.na(cbd_suburb)) |>
    openxlsx::write.xlsx(
        file.path(
            output_path,
            "descriptives/avg_counts_consumption_amenities.xlsx"
        )
    )


#--------------------------------------------------
# plot indices across all times

index_data <- list(environ_index, cons_index)

# loop through data
for (dta in index_data) {
    # get index variables
    index_vars <- dta |>
        select(contains("index")) |>
        names()

    # only keep combined index for consumption amenities
    if ("food_index" %in% names(dta)) {
        index_vars <- c("urbam_index")
    } else {
        index_vars <- c("environ_area_index")
    }

    # merge zipcode distances
    dta <- merge(
        dta,
        zipcodes,
        by = "unique_plz",
        all.x = TRUE
    )

    # loop through variables
    for (index_var in index_vars) {
        plot <- ggplot(
            data = dta |> filter(dist_inhab <= 25000),
            aes(x = dist_inhab / 1000, y = .data[[index_var]])
        ) +
            geom_point() +
            geom_smooth(method = "lm", col = "blue", linewidth = 1.25) +
            labs(
                x = "Distance to the CBD (km)",
                y = "Amenity index"
            ) +
            theme_light() +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 18)
            )

        ggsave(
            plot = plot,
            file.path(
                output_path,
                "figs",
                paste0(index_var, "_CBD_distance.png")
            ),
            dpi = 300
        )
    }
}

#--------------------------------------------------
# plot indices with respect to hedonic values

# get hedonic types and amenity index variables
hedonic_types <- unique(housing_amenity$var)
index_vars <- housing_amenity |>
    select(urbam_index, environ_area_index) |>
    names()

# comparison between 2020 and 2021 across all zipcodes independent of CBD/suburb
for (hedonic_type in hedonic_types) {
    for (index_var in index_vars) {
        # define labels for x-axis
        if (hedonic_type == "lnhri") {
            xlabel <- "lnR"
        } else {
            xlabel <- "lnP"
        }

        # generate plot
        plot <- ggplot() +
            geom_point(
                data = housing_amenity |>
                    filter(var == hedonic_type & time == "2020-03-01"),
                aes(x = val, y = .data[[index_var]], col = "2020"),
                alpha = 0.1,
                show.legend = FALSE
            ) +
            geom_smooth(
                data = housing_amenity |>
                    filter(var == hedonic_type & time == "2020-03-01"),
                aes(x = val, y = .data[[index_var]], col = "2020"),
                method = "lm",
                se = FALSE,
                linewidth = 1.25
            ) +
            geom_point(
                data = housing_amenity |>
                    filter(var == hedonic_type & time == "2021-03-01"),
                aes(x = val, y = .data[[index_var]], col = "2021"),
                alpha = 0.1,
                show.legend = FALSE
            ) +
            geom_smooth(
                data = housing_amenity |>
                    filter(var == hedonic_type & time == "2021-03-01"),
                aes(x = val, y = .data[[index_var]], col = "2021"),
                method = "lm",
                se = FALSE,
                linewidth = 1.25
            ) +
            scale_color_manual(
                values = c(
                    "2020" = cols[1],
                    "2021" = cols[2]
                ),
                name = ""
            ) +
            labs(
                x = xlabel,
                y = "Amenity index"
            ) +
            theme_light() +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 18),
                legend.position = "bottom",
                legend.key.size = unit(1, "cm"),
                legend.text = element_text(size = 18)
            )

        # export
        filename <- paste0(hedonic_type, "_", index_var, ".png")
        ggsave(
            plot = plot,
            file.path(
                output_path,
                "figs",
                filename
            ),
            dpi = 300
        )
    }
}

# comparison between 2020 and 2021 dependent on CBD/ suburb
locs <- c("suburb", "cbd")

for (loc in locs) {
    for (hedonic_type in hedonic_types) {
        for (index_var in index_vars) {
            # define labels for x-axis
            if (hedonic_type == "lnhri") {
                xlabel <- "lnR"
            } else {
                xlabel <- "lnP"
            }

            # subset for suburbs combined (ring 1 + ring 2)
            if (loc == "suburb") {
                dta <- housing_amenity |>
                    filter(cbd_suburb %in% c("suburb_ring_1", "suburb_ring_2"))
            } else {
                dta <- housing_amenity |>
                    filter(cbd_suburb == loc)

            }

            # generate plot
            plot <- ggplot() +
                geom_point(
                    data = dta |>
                        filter(var == hedonic_type & time == "2020-03-01"),
                    aes(x = val, y = .data[[index_var]], col = "2020"),
                    alpha = 0.1,
                    show.legend = FALSE
                ) +
                geom_smooth(
                    data = dta |>
                        filter(var == hedonic_type & time == "2020-03-01"),
                    aes(x = val, y = .data[[index_var]], col = "2020"),
                    method = "lm",
                    se = FALSE,
                    linewidth = 1.25
                ) +
                geom_point(
                    data = dta |>
                        filter(var == hedonic_type & time == "2021-03-01"),
                    aes(x = val, y = .data[[index_var]], col = "2021"),
                    alpha = 0.1,
                    show.legend = FALSE
                ) +
                geom_smooth(
                    data = dta |>
                        filter(var == hedonic_type & time == "2021-03-01"),
                    aes(x = val, y = .data[[index_var]], col = "2021"),
                    method = "lm",
                    se = FALSE,
                    linewidth = 1.25
                ) +
                scale_color_manual(
                    values = c(
                        "2020" = cols[1],
                        "2021" = cols[2]
                    ),
                    name = ""
                ) +
                labs(
                    x = xlabel,
                    y = "Amenity index"
                ) +
                theme_light() +
                theme(
                    axis.text = element_text(size = 16),
                    axis.title = element_text(size = 18),
                    legend.position = "bottom",
                    legend.key.size = unit(1, "cm"),
                    legend.text = element_text(size = 18)
                )

            # export
            filename <- paste0(hedonic_type, "_", index_var, "_", loc, ".png")
            ggsave(
                plot = plot,
                file.path(
                    output_path,
                    "figs",
                    filename
                ),
                dpi = 300
            )
        }
    }
}

#--------------------------------------------------
# estimating the relationship between amenity indices and distance

# merge distance to indices
environ_index <- environ_index |>
    merge(
        zipcodes,
        by = "unique_plz",
        all.x = TRUE
    )

cons_index <- cons_index |>
    merge(
        zipcodes,
        by = "unique_plz",
        all.x = TRUE
    )

# estimate environmental index to distance
est_environ <- feols(
    environ_area_index ~ dist_inhab,
    data = environ_index,
    se = "hetero"
)

est_cons <- feols(
    urbam_index ~ dist_inhab,
    data = cons_index,
    se = "hetero"
)

# export
etable(
    est_environ, est_cons,
    headers = c("ENVIRON", "CONS")
)

esttex(
    est_environ, est_cons,
    file = file.path(
        output_path,
        "tabs/est_parameters_indices.tex"
    ),
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    se = "hetero",
    headers = c("ENVIRON", "CONS"),
    replace = TRUE
)

#--------------------------------------------------
# estimating the effect of pandemic on amenity valuation

# add logarithmic information for controls
housing_amenity <- housing_amenity |>
    mutate(
        lndist = log(1 + dist_inhab / 1000),
        lnpop_dens = log(pop_mun / area_qkm),
        lnpop = log(1 + inhabitants),
        lnpp = log(1 + purchase_power),
    )

# add pandemic dummy
housing_amenity <- housing_amenity |>
    mutate(
        pandemic = as.factor(case_when(
            time >= "2020-03-01" ~ 1,
            TRUE ~ 0
        ))
    )

# define controls
controls <- c("lnpp + share_male_1845 + share_background_german")

# define estimation function
est_function <- function(data, hedonic = c("lnhpi", "lnhri"), index_var) {
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

    fml <- formula(
        paste0(
            "val ~", index_var, "* pandemic +", controls, "| amr_name + year_mon"
        )
    )

    # estimate model
    est_mod <- feols(
        fml = fml,
        data = dta,
        se = "hetero"
    )

    # return output
    return(est_mod)

}

# define estimation options
hedonic_types <- c("lnhpi", "lnhri")
index_vars <- housing_amenity |>
    select(contains("index")) |>
    select(!contains("people")) |>
    names()

# define function that runs estimation by looping through options
# and exports data
run_and_export <- function(housing_data, name_for_export) {
    # list for storage
    est_results_list <- list()

    # loop through options
    for (hedonic_type in hedonic_types) {
        for (index_var in index_vars) {
            # estimate model given inputs
            mod <- est_function(
                data = housing_data,
                hedonic = hedonic_type,
                index = index_var
            )

            # save results
            result_name <- paste(
                toupper(hedonic_type),
                toupper(index_var),
                sep = "-"
            )
            est_results_list[[result_name]] <- mod
        }
    }

    # export findings
    for (hedonic_type in toupper(hedonic_types)) {
        esttex(
            est_results_list[[paste0(hedonic_type, "-FOOD_INDEX")]],
            est_results_list[[paste0(hedonic_type, "-EDUCATION_INDEX")]],
            est_results_list[[paste0(hedonic_type, "-ENTERTAINMENT_INDEX")]],
            est_results_list[[paste0(hedonic_type, "-URBAM_INDEX")]],
            est_results_list[[paste0(hedonic_type, "-ENVIRON_AREA_INDEX")]],
            est_results_list[[paste0(hedonic_type, "-WATER_AREA_INDEX")]],
            est_results_list[[paste0(hedonic_type, "-GREEN_AREA_INDEX")]],
            signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
            headers = hedonic_type,
            se = "hetero",
            replace = TRUE,
            file = file.path(
                output_path,
                "tabs",
                paste0(name_for_export, "_amenity_pandemic_effect_", hedonic_type, ".tex")
            )
        )
    }
}

# export findings
run_and_export(housing_data = housing_amenity, name_for_export = "all")

#--------------------------------------------------
# estimation for CBD

# subset for CBDs
cbds <- housing_amenity |>
    filter(cbd_suburb == "cbd")

# estimation
run_and_export(housing_data = cbds, name_for_export = "cbd")

#--------------------------------------------------
# estimation for suburb ring 1

# subset for suburb ring 1
suburb1 <- housing_amenity |>
    filter(cbd_suburb == "suburb_ring_1")

# estimation
run_and_export(housing_data = suburb1, name_for_export = "suburb_1")

#--------------------------------------------------
# estimation for suburb ring 2

# subset for suburb ring 2
suburb2 <- housing_amenity |>
    filter(cbd_suburb == "suburb_ring_2")

# estimation
run_and_export(housing_data = suburb1, name_for_export = "suburb_2")
