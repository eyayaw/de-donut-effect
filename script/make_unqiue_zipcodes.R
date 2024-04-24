library(sf)
library(stringr)

## import shapes ----
grids = st_read("./data/geodata/germany-grid/de-grid.gpkg", quiet = TRUE)
zipcodes = st_read("./data/geodata/zip-codes/plz-5stellig.geojson", quiet = TRUE)
municip = st_read("./data/geodata/admin-areas/municipalities.gpkg", quiet = TRUE)


## cleaning ----
st_geometry(grids) = "geometry"
st_geometry(municip) = "geometry"
municip = municip[, c("ags", "geo_name", "geometry")]

municip$ags = sprintf("%08i", as.integer(municip$ags))
zipcodes$plz = sprintf("%05i", as.integer(zipcodes$plz))

# transform all other projections into
main_crs = st_crs(municip)
grids = st_transform(grids, main_crs)
zipcodes = st_transform(zipcodes, main_crs)

# zipcodes are not unique
zipcodes$note = sub("^\\d{4,5} ", "", zipcodes$note) # remove the zipcode from the note
# number of associated towns
zipcodes$num_towns = lengths(strsplit(zipcodes$note, ", (?! ?u\\.a\\.?)", perl = TRUE))

zips_scattered = which(zipcodes$num_towns > 1)
zipcodes_not_unique = zipcodes[zips_scattered, ]

# subset for zipcodes that are okay i.e. associated with only one town
zipcodes_ok <- zipcodes |>
  subset(!(plz %in% zipcodes_not_unique$plz))

# join city to zipcodes_ok
zipcodes_cities <- st_join(zipcodes_ok, municip, left = TRUE, largest = TRUE)

# create a var that serves as a unique identifier
zipcodes_cities <- zipcodes_cities |>
  transform(unique_plz = paste0(plz, "-", ags))

# clean city names in note
zipcodes_not_unique$note <- gsub(" u.a.", "", zipcodes_not_unique$note)

# empty list for storage
zipcodes_made_unique_list <- list()

finding_unique_zipcodes <- function(row) {
  # extract row
  row_data <- zipcodes_not_unique[row, ]

  # split note (i.e. cities)
  # delete trailing white space
  cit <- str_trim(str_split_1(unique(row_data$note), ","))

  # filter municipality data for cities
  cities <- municip |>
    subset(geo_name %in% cit)

  # intersect zip code and municipalities
  inter <- suppressWarnings(st_intersection(row_data, cities))

  if (nrow(inter) == 0) {
    message(sprintf("The geometries of zipcode(s) not intersecting with the associated municipality(ies) found for plz `%s`.", row_data$plz))
    message(sprintf("Potentially associated with [%s].", paste(cities$ags, collapse = ",")))
    return()
  }
  # some cleaning
  inter <- inter |>
    transform(unique_plz = paste0(plz, "-", ags)) |>
    st_cast("MULTIPOLYGON")

  inter
}

for (i in seq(1, nrow(zipcodes_not_unique))) {
  zipcodes_made_unique_list[[i]] <- finding_unique_zipcodes(row = i)
}

zipcodes_made_unique <- data.table::rbindlist(zipcodes_made_unique_list) |> st_as_sf()

# combine everything
zipcodes_unique <- rbind(zipcodes_cities, zipcodes_made_unique)
# compute the area of the zipcodes
zipcodes_unique$area_qkm = st_area(zipcodes_unique) |> units::set_units(km^2)
# export
st_write(zipcodes_unique, "./data/geodata/zip-codes/unique_zipcodes_municipality.gpkg", append = FALSE)

