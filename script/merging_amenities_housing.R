# This file merges the housing data (hedonic indices) and the normalized
# consumption (= urban) and environmental amenities

# Generates main file: housing_amenities.csv

#--------------------------------------------------
# paths

main_path <- getwd()
data_path <- file.path(main_path, "data")

#--------------------------------------------------
# libraries

library(data.table)
library(dplyr)
library(fixest)
library(tidyr)
library(stringr)

#--------------------------------------------------
# load data

housing <- data.table::fread(
    file.path(
        data_path,
        "processed/final_indexes.csv"
    )
)

# consumption amenities
cons_amen <- data.table::fread(
    file.path(
        data_path,
        "processed/urban_amenities_normalized.csv"
    )
)

# environmental amenities
environ_amen <- data.table::fread(
    file.path(
        data_path,
        "processed/water_green_amenities_normalized.csv"
    )
)

#--------------------------------------------------
# restrict housing data to 2017 and onwards
# since OSM data starts only in 2017

housing_restricted <- housing |>
    filter(year >= 2017)

#--------------------------------------------------
# keep only normalized indices

cons_amen_index <- cons_amen |>
    select(unique_plz, contains("norm"))

environ_amen_index <- environ_amen |>
    select(unique_plz, contains("norm"))

#--------------------------------------------------
# make consumption amenities wide format

cons_amen_index_wide <- cons_amen_index |>
    tidyr::pivot_longer(
        cols = !unique_plz,
        values_to = "consumption_index",
        names_to = "amen_variable"
    ) |>
    mutate(
        # add year variable
        year = as.numeric(
            substring(
                amen_variable,
                first = nchar(amen_variable) - 3,
                last = nchar(amen_variable)
            )
        ),
        # get the names of the single groups going into consumption index
        cat = str_split_fixed(amen_variable, pattern = "_", n = 4)[, 2],
        # replace "norm" which is a placeholder for combined consumption index
        cat = case_when(
            cat == "norm" ~ "urbam_index",
            TRUE ~ paste0(cat, "_index")
        )
    ) |>
    # drop original amenity variable
    select(-amen_variable) |>
    # make wide format again (one row per year and zipcode for each 4 consumption index components)
    tidyr::pivot_wider(
        names_from = "cat",
        values_from = "consumption_index"
    )

#--------------------------------------------------
# make environmental amenities wide format

environ_amen_index_wide <- environ_amen_index |>
    tidyr::pivot_longer(
        cols = !unique_plz,
        values_to = "environ_index",
        names_to = "amen_variable"
    ) |>
    mutate(
        # add single groups
        cat = substring(
            amen_variable,
            first = nchar(amen_variable) - 4,
            last = nchar(amen_variable)
        ),
        # replace placeholder for combined index
        cat = case_when(
            cat != "green" & cat != "water" ~ "environ",
            TRUE ~ cat
        ),
        # extract group (people, or area)
        group = str_split_fixed(amen_variable, pattern = "_", n = 5)[, 4],
        # add naming variable (for pivot_wider)
        names = paste0(cat, "_", group, "_index"),
        # add year
        year = as.numeric(
            str_extract(amen_variable, pattern = "[0-9]+")
        )
    ) |>
    # move year to second position
    relocate(year, .after = unique_plz) |>
    # drop original amenity variable
    select(-c(amen_variable, cat, group)) |>
    # make wide format again
    tidyr::pivot_wider(
        names_from = "names",
        values_from = "environ_index"
    )

#--------------------------------------------------
# combine amenity data with housing

amenity_housing <- merge(
    housing_restricted,
    cons_amen_index_wide,
    by = c("unique_plz", "year"),
    all.x = TRUE
) |>
merge(
    environ_amen_index_wide,
    by = c("unique_plz", "year"),
    all.x = TRUE
)

#--------------------------------------------------
# export

data.table::fwrite(
    cons_amen_index_wide,
    file.path(
        data_path,
        "processed/cons_amenities_normalized_wide.csv"
    )
)

# export
data.table::fwrite(
    environ_amen_index_wide,
    file.path(
        data_path,
        "processed/environ_amenities_normalized_wide.csv"
    )
)

data.table::fwrite(
    amenity_housing,
    file.path(
        data_path,
        "processed/housing_amenities.csv"
    )
)
