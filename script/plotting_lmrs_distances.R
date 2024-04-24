# This file plots the CBDs (based on LMRs) and the distance to the CBD (which
# is the distance between zip-code center and the CBD).

#--------------------------------------------------
# paths

main_path <- getwd()
data_path <- file.path(main_path, "data")
output_path <- file.path(main_path, "output")

#--------------------------------------------------
# libraries

library(data.table)
library(ggplot2)
library(sf)

#--------------------------------------------------
# load data

# zipcode data
zipcodes <- fread(
  file.path(data_path, "geodata/zip-codes/zip-codes_dist2cbd.csv"),
  select = c("unique_plz", "dist_inhab")
)

# geo info of zipcodes
zipcodes_geo <- st_read(
  file.path(data_path, "geodata/zip-codes/unique_zipcodes_municipality.gpkg"),
  quiet = TRUE
)[, c("unique_plz")]

# CBDs
cbds <- fread(
  file.path(data_path, "geodata/CBDs/lmrs_centroids.csv"),
  select = c("lon_geo", "lat_geo")
)

#--------------------------------------------------
# make CBDs spatial
cbds <- st_as_sf(cbds, coords = c("lon_geo", "lat_geo"), crs = 25832)


#--------------------------------------------------
# merge geo info to zipcodes

zipcodes_sf <- merge(zipcodes, zipcodes_geo, by = "unique_plz") |> st_as_sf()

# rename geometry column
st_geometry(zipcodes_sf) <- "geometry"

#--------------------------------------------------
# generate map

dist_map <- ggplot() +
  geom_sf(
    data = zipcodes_sf,
    aes(geometry = geometry, fill = dist_inhab / 1000), col = NA
  ) +
  geom_sf(
    data = zipcodes_sf, aes(geometry = geometry), col = "grey90", fill = NA
  ) +
  geom_sf(
    data = cbds, aes(geometry = geometry), fill = NA, col = "black"
  ) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, breaks = seq(0, 160, 40),
    name = "Distance to\nthe CBD"
  ) +
  theme_void(12)


# export map
ggsave(
  file.path(output_path, "figs/map_cbd_distance.png"), dist_map,
  width = 3024, height = 3024, units = "px"
)
