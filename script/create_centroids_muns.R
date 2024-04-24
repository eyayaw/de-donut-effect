#--------------------------------------------------
# import libraries

library(data.table)
library(sf)


#--------------------------------------------------
# load data

# grid shapes
grids <- st_read(
    "./data/geodata/germany-grid/de-grid.gpkg",
    quiet = TRUE
)

# municipality shapes
municip <- st_read(
    "./data/geodata/admin-areas/municipalities.gpkg", quiet = TRUE
)

# RWI-GEO-GRID information
microm <- data.table::fread("./data/processed/microm-tidy.csv")

#--------------------------------------------------
# merge geo of grids and microm data

microm_geo <- merge(
    subset(microm, year == 2019), grids, by = "grid_id", all.x = TRUE
)

# rename geometry
names(microm_geo)[names(microm_geo) == "geom"] <- "geometry"

# set geometry
microm_geo <- st_set_geometry(microm_geo, microm_geo$geometry)

# transfrom
microm_geo <- st_transform(microm_geo, crs = st_crs(municip))

#--------------------------------------------------
# add municipality information

city_grids <- st_join(microm_geo, municip, left = TRUE, largest = TRUE)

# set geometry
city_grids <- st_set_geometry(city_grids, city_grids$geometry)

#--------------------------------------------------
# weighted centroid on municipality level

# exam <- city_grids |>
#     filter(geo_name == "Berlin")

# find centroid for each grid
grid_centroids <- st_centroid(city_grids)

# extract coordinates for each grid centroid
city_grids$lon_centroid <- st_coordinates(grid_centroids)[, 1]
city_grids$lat_centroid <- st_coordinates(grid_centroids)[, 2]

# add weighted coordinates
grid_centroids <- city_grids |>
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
weighted_center <- grid_centroids |>
    st_drop_geometry() |>
    DT(x = _, , .(
        weighted_lon_inhab = sum(aux_lon_inhab, na.rm = TRUE) / sum(inhabitants, na.rm = TRUE),
        weighted_lat_inhab = sum(aux_lat_inhab, na.rm = TRUE) / sum(inhabitants, na.rm = TRUE),
        weighted_lon_resid_build = sum(aux_lon_resid_build, na.rm = TRUE) / sum(resid_buildings, na.rm = TRUE),
        weighted_lat_resid_build = sum(aux_lat_resid_build, na.rm = TRUE) / sum(resid_buildings, na.rm = TRUE),
        weighted_lon_num_hhs = sum(aux_lon_num_hhs, na.rm = TRUE) / sum(num_hhs, na.rm = TRUE),
        weighted_lat_num_hhs = sum(aux_lat_num_hhs, na.rm = TRUE) / sum(num_hhs, na.rm = TRUE)
    ), ags)

#--------------------------------------------------
# export
fwrite(
    weighted_center, "./data/geodata/CBDs/weighted_centroids_municipalities.csv"
)
