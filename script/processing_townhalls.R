#--------------------------------------------------
# libraries

library(data.table)
library(dplyr)
library(stringi)
library(sf)

#--------------------------------------------------
# load data

# townhall information
townhalls <- fread(
    "./data/geodata/townhalls/townhalls_germany_prep.shp"
)

# municipality shape files
municip <- st_read(
    "./data/geodata/admin-areas/municipalities.gpkg",
    quiet = TRUE
)

#--------------------------------------------------
# drop unnessary information

townhalls <- townhalls |>
    select(
        osmid, name, geometry
    ) |>
    mutate(
        # adjust names
        name = stringi::stri_trans_general(name, "de-ASCII; LATIN-ASCII")
    )

# set as spatial data
townhalls <- st_set_geometry(townhalls, townhalls$geometry)

# tranform
townhalls <- st_transform(townhalls, crs = st_crs(municip))

#--------------------------------------------------
# assign municipalities to townhalls

townhalls_municip <- st_join(
    townhalls,
    municip,
    left = TRUE,
    largest = TRUE
)

#--------------------------------------------------
# add number of townhalls per city

townhalls_municip <- townhalls_municip |>
    group_by(ags) |>
    mutate(
        num_townhalls_city = n()
    )

#--------------------------------------------------
# single out "Rathaus"

rathaus <- townhalls_municip |>
    filter(stringr::str_detect(name, "Rathaus|rathaus"))

#--------------------------------------------------
# calculate centroids if multiple buildings are located within the city

townhall_centroids <- townhalls_municip |>
    group_by(ags) |>
    summarise(geometry = st_union(geometry)) |>
    st_centroid()

rathaus_centroids <- rathaus |>
    group_by(ags) |>
    summarise(geometry = st_union(geometry)) |>
    st_centroid()

#--------------------------------------------------
# export

st_write(
    townhall_centroids,
    "./data/geodata/townhalls/townhall_centroids.gpkg"
)

st_write(
    rathaus_centroids,
    "./data/geodata/townhalls/rathaus_centroids.gpkg"
)
