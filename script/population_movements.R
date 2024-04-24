# This function plots the population and population change in CBD and suburb
# based on RWI-GEO-GRID (v13).

#--------------------------------------------------
# paths

main_path <- getwd()
data_path <- file.path(main_path, "data")
output_path <- file.path(main_path, "output")

#--------------------------------------------------
# libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(stringi)

#--------------------------------------------------
# read data

microm <- fread(
    file.path(
        data_path,
        "processed/microm-tidy-v13.csv"
    )
)

grids <- st_read(
    file.path(
        data_path,
        "geodata/germany-grid/de-grid.gpkg"
    ),
    quiet = TRUE
)

zipcodes <- st_read(
    file.path(
        data_path,
        "geodata/zip-codes/unique_zipcodes_municipality.gpkg"
    ),
    quiet = TRUE
)

zipocdes_distances <- fread(
    file.path(
        data_path,
        "geodata/zip-codes/zip-codes_dist2cbd.csv"
    )
)

moving <- fread(
    file.path(
        data_path,
        "population/fort_zu_zuege_gemeinden.csv"
    )
)

#--------------------------------------------------
# make same CRS

zipcodes <- st_transform(zipcodes, crs = st_crs(grids))

#--------------------------------------------------
# bring zipcodes and grids together

zipcodes_grids <- st_join(
    grids,
    zipcodes,
    largest = TRUE,
    left = TRUE
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
        zipocdes_distances$dist_inhab,
        prob = seq(0, 1, 0.1),
        na.rm = TRUE
    )
)

# classify zipcodes
zipocdes_distances <- zipocdes_distances |>
    mutate(
        cbd_suburb = case_when(
            dist_inhab <= dist_quants[2] ~ "cbd",
            dist_inhab > dist_quants[2] & dist_inhab <= dist_quants[4] ~ "suburb_ring_1",
            dist_inhab > dist_quants[4] & dist_inhab <= dist_quants[6] ~ "suburb_ring_2",
            TRUE ~ "outside"
        )
    )

#--------------------------------------------------
# merge grid info and zipcode distances

grids_with_distances <- merge(
    zipcodes_grids |>
        st_drop_geometry() |>
        select(grid_id, unique_plz),
    zipocdes_distances,
    by = "unique_plz"
)

#--------------------------------------------------
# merge grid info to microm

microm_prep <- merge(
    microm,
    grids_with_distances,
    by = "grid_id",
    left = TRUE
)

#--------------------------------------------------
# trend of population in CBD and suburbs

summary_pop_cbd_suburb <- microm_prep |>
    group_by(year, cbd_suburb) |>
    # do not consider zipcodes beyond the suburb
    filter(cbd_suburb != "outside") |>
    summarise(
        total_pop = sum(inhabitants, na.rm = TRUE)
    ) |>
    # add the change in population relation to 2019
    group_by(cbd_suburb) |>
    mutate(
        change_pop_to_2019 = total_pop - total_pop[year == 2019]
    )

# define colors
cols <- c("#2b8043", "#1746A2", "#c70002")

# generate plot for overall trend
overall_pop_plot <- ggplot()+
    geom_line(
        data = summary_pop_cbd_suburb,
        aes(
            x = year,
            y = total_pop,
            group = cbd_suburb,
            col = cbd_suburb
        ),
        linewidth = 1
    )+
    labs(
        x = "",
        y = "Total population"
    )+
    scale_y_continuous(
        labels = scales::comma
    )+
    scale_color_manual(
        values = c(
            "cbd" = cols[3],
            "suburb_ring_1" = cols[1],
            "suburb_ring_2" = cols[2]
        ),
        labels = c(
            "cbd" = "CBD",
            "suburb_ring_1" = expression(Suburb["Ring 1"]),
            "suburb_ring_2" = expression(Suburb["Ring 2"])
        ),
        name = ""
    )+
    geom_vline(
        xintercept = 2020,
        linewidth = 0.8,
        linetype = "dashed",
        col = "black"
    )+
    theme_light()+
    theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 20)
    )

ggsave(
    plot = overall_pop_plot,
    file.path(
        output_path,
        "figs",
        "population_trends.png"
    )
)

# generate plot for change relative to 2019
change_pop_plot <- ggplot()+
    geom_line(
        data = summary_pop_cbd_suburb,
        aes(
            x = year,
            y = change_pop_to_2019,
            group = cbd_suburb,
            col = cbd_suburb
        ),
        linewidth = 1
    )+
    labs(
        x = "",
        y = "Population change (rel. to 2019)"
    )+
    scale_y_continuous(
        labels = scales::comma
    )+
    scale_color_manual(
        values = c(
            "cbd" = cols[3],
            "suburb_ring_1" = cols[1],
            "suburb_ring_2" = cols[2]
        ),
        labels = c(
            "cbd" = "CBD",
            "suburb_ring_1" = expression(Suburb["Ring 1"]),
            "suburb_ring_2" = expression(Suburb["Ring 2"])
        ),
        name = ""
    )+
    geom_vline(
        xintercept = 2020,
        linewidth = 0.8,
        linetype = "dashed",
        col = "black"
    )+
    theme_light()+
    theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 20)
    )

ggsave(
    plot = change_pop_plot,
    file.path(
        output_path,
        "figs",
        "change_population_2019.png"
    )
)

#--------------------------------------------------
# change to 2019 on the grid-level

change_time <- microm_prep |>
    group_by(grid_id) |>
    mutate(
        # compute difference only if all 5 years are included for grid
        # some grids have missing years
        length_year = length(year)
    ) |>
    filter(length_year == 5) |>
    mutate(
        pop_change_2019 = inhabitants - inhabitants[year == 2019]
    ) |>
    select(-length_year)

# calculate the average population change on the grid-level
sum_change_time <- change_time |>
    group_by(cbd_suburb, year) |>
    summarise(
        mean_change = mean(pop_change_2019, na.rm = TRUE)
    ) |>
    # do not consider outside areas
    filter(cbd_suburb != "outside") |>
    tidyr::pivot_wider(
        id_cols = "cbd_suburb",
        names_from = "year",
        values_from = "mean_change"
    )

# export
openxlsx::write.xlsx(
    sum_change_time,
    file.path(
        output_path,
        "descriptives/average_population_change_grids.xlsx"
    ),
    rowNames = FALSE
)

#--------------------------------------------------
# clean moving data

moving_prep <- moving |>
    select(
        ags,
        year,
        municipality = kreis_name,
        move_in = category,
        move_out = V4
    ) |>
    # replace Umlaute
    mutate(
        municipality = stringi::stri_trans_general(municipality, "de-ASCII; Latin-ASCII")
    ) |>
    # drop all observations that are on national-level
    filter(nchar(ags) >= 3) |>
    # drop subnational levels (Regierungsbezirke)
    filter(nchar(ags) >= 5) |>
    mutate(
        # replace Kreisfreie Stadt label
        municipality = case_when(
            stringr::str_detect(municipality, "Kreisfreie Stadt") ~ stringr::str_replace(municipality, "Kreisfreie Stadt", ""),
            TRUE ~ municipality
        ),
        # make moving statistics numeric
        move_in = as.numeric(move_in),
        move_out = as.numeric(move_out),
        # calculate the net migration balance
        net_migration = move_in - move_out
    ) |>
    # drop any districts
    filter(stringr::str_detect(municipality, "Kreis") == FALSE) |>
    filter(stringr::str_detect(municipality, "Landkreis") == FALSE) |>
    # drop anything that does not belong to a municipality
    filter(stringr::str_detect(municipality, "gemeindefreies Gebiet") == FALSE) |>
    # drop Regionalverband
    filter(stringr::str_detect(municipality, "Regionalverband") == FALSE) |>
    # drop any rows that have both missings in moving numbers
    filter(!is.na(move_in) & !is.na(move_out))

# calculate the average moving statistics
sum_migration <- moving_prep |>
    group_by(year) |>
    summarise(
        mean_move_in = mean(move_in, na.rm = TRUE),
        mean_move_out = mean(move_out, na.rm = TRUE),
        mean_net_migration = mean(net_migration, na.rm = TRUE)
    )

# export table
openxlsx::write.xlsx(
    sum_migration,
    file.path(
        output_path,
        "descriptives/average_moving_stats.xlsx"
    ),
    rowNames = FALSE
)

