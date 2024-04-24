#--------------------------------------------------
# import libraries

library(data.table)
library(sf)
library(ggplot2)

#--------------------------------------------------
# load data

# grid shapes
grids <- st_read(
    "./data/geodata/germany-grid/de-grid.gpkg",
    quiet = TRUE
)

# LMR shapes
lmrs <- st_read(
    "./data/geodata/labor-market-regions/Labor-Market-Regions_Kosfeld-Werner-2012_2019.gpkg",
    quiet = TRUE
)

# RWI-GEO-GRID information
microm <- data.table::fread(
    "./data/processed/microm-tidy.csv"
)

#--------------------------------------------------
# merge geo of grids and microm data

microm_geo <- merge(
    subset(microm, year == 2019), grids, by = "grid_id", all.x = TRUE
) |> st_as_sf()

# rename geometry
st_geometry(microm_geo) <- "geometry"


# transfrom
microm_geo <- st_transform(microm_geo, crs = st_crs(lmrs))

#--------------------------------------------------
# add municipality information

lmr_grids <- st_join(microm_geo, lmrs, largest = TRUE) # left = T will produce grids with NA amr_name


#--------------------------------------------------
# weighted centroid on LMR level

# find centroid for each grid
grid_centroids <- st_centroid(lmr_grids)

# extract coordinates for each grid centroid
grid_centroids[, c("lon_centroid", "lat_centroid")] <- unname(st_coordinates(grid_centroids))

# add weighted coordinates
grid_centroids <- grid_centroids |>
    st_drop_geometry() |>
    transform(
        aux_lon_inhab = inhabitants * lon_centroid,
        aux_lat_inhab = inhabitants * lat_centroid,
        aux_lon_resid_build = resid_buildings * lon_centroid,
        aux_lat_resid_build = resid_buildings * lat_centroid,
        aux_lon_num_hhs = num_hhs * lon_centroid,
        aux_lat_num_hhs = num_hhs * lat_centroid
    )

# calculate weighted centroids by municipalitiy
weighted_cents <- as.data.table(grid_centroids)[,
    .(
        weighted_lon_inhab = sum(aux_lon_inhab, na.rm = TRUE) / sum(inhabitants, na.rm = TRUE),
        weighted_lat_inhab = sum(aux_lat_inhab, na.rm = TRUE) / sum(inhabitants, na.rm = TRUE),
        weighted_lon_resid_build = sum(aux_lon_resid_build, na.rm = TRUE) / sum(resid_buildings, na.rm = TRUE),
        weighted_lat_resid_build = sum(aux_lat_resid_build, na.rm = TRUE) / sum(resid_buildings, na.rm = TRUE),
        weighted_lon_num_hhs = sum(aux_lon_num_hhs, na.rm = TRUE) / sum(num_hhs, na.rm = TRUE),
        weighted_lat_num_hhs = sum(aux_lat_num_hhs, na.rm = TRUE) / sum(num_hhs, na.rm = TRUE)
    ), amr_name
]


# add the labor market shapes for the unweighted/geo centriods
geo_cents = lmrs |>
    st_centroid() |>
    {
        \(.x) cbind(
            st_drop_geometry(.x[, c("amr_id", "amr_name")]),
            st_coordinates(.x)[, c("X", "Y")] |>
            as.data.frame() |>
                setNames(c("lon_geo", "lat_geo"))
        )
    }()


# centriods of the labor market regions: geo + weighted coordinates
cents = merge(geo_cents, weighted_cents)

#--------------------------------------------------
# export

data.table::fwrite(cents, "./data/geodata/CBDs/lmrs_centroids.csv")


#--------------------------------------------------
# plot for Berlin

# filter centers for Berlin
berlin_centers <- weighted_cents |>
    subset(amr_name == "Berlin") |>
    st_drop_geometry()

# set geometries for center
berlin_inhab_center <- st_as_sf(berlin_centers, coords = c("weighted_lon_inhab", "weighted_lat_inhab"), crs = st_crs(lmrs))
berlin_resid_center <- st_as_sf(berlin_centers, coords = c("weighted_lon_resid_build", "weighted_lat_resid_build"), crs = st_crs(lmrs))
berlin_hhs_center <- st_as_sf(berlin_centers, coords = c("weighted_lon_num_hhs", "weighted_lat_num_hhs"), crs = st_crs(lmrs))

# calculate geographical center
berlin_geo_center <- lmrs |>
    subset(amr_name == "Berlin") |> st_centroid()

# plot all together
pal <- MetBrewer::met.brewer(name = "Archambault", n = 7)

berlin_plot <- ggplot() +
    geom_sf(
        data = lmrs |>
            subset(amr_name == "Berlin"),
        aes(geometry = geom),
        fill = NA
    ) +
    geom_sf(
        data = berlin_geo_center,
        aes(geometry = geom, color = "geo"),
        size = 3
    ) +
    geom_sf(
        data = berlin_inhab_center,
        aes(geometry = geometry, color = "inhab"),
        size = 3
    ) +
    geom_sf(
        data = berlin_resid_center,
        aes(geometry = geometry, color = "resid"),
        size = 3
    ) +
    geom_sf(
        data = berlin_hhs_center,
        aes(geometry = geometry, color = "hhs"),
        size = 3
    ) +
    scale_color_manual(
        values = c(
            "geo" = "red",
            "inhab" = "green",
            "resid" = "blue",
            "hhs" = "orange"
        ),
        labels = c(
            "geo" = "None (geographical)",
            "inhab" = "Population",
            "resid" = "Resid. buildings",
            "hhs" = "Number of households"
        ),
        name = "Center weighted by"
    ) +
    theme_void() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)
    )

ggsave(
    plot = berlin_plot,
    "./output/figs/berlin_cbds_weighted.png",
    dpi = 300
)
