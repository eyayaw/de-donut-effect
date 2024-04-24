#--------------------------------------------------
# import libraries
library(sf)
library(data.table)
source("./script/helpers/helpers.R")
#--------------------------------------------------
# load data

# centroids of unique zipcodes with their labor market regions
zip_cents = st_read("./data/geodata/zip-codes/xwalk_zipcodes-lmrs.gpkg") |> st_centroid()

# LMR centroids
cents_lmrs = fread("./data/geodata/CBDs/lmrs_centroids.csv")

# reshape to long
## rename so that stats::reshape works smoothly
names(cents_lmrs) = sub("(?:weighted_)?(lon|lat)_", "\\1.", names(cents_lmrs))

cents_lmrs_long = reshape(
  cents_lmrs,
  direction = "long", idvar = c("amr_name", "amr_id"),
  varying = grep("^(lon|lat).", names(cents_lmrs), value = T),
  timevar = "cent_type"
)


#--------------------------------------------------
# define sf for different CBD definitions

cents_lmrs_long = st_as_sf(cents_lmrs_long, coords = c("lon", "lat"), crs = st_crs(zip_cents))

#--------------------------------------------------
# calculate distance to CBD (on LMR level)
cbd_by = "amr_name"


## distance to weighted cbds ----
calculate_dist <- function(cents_data, cent_type) {
  cbds = cents_data[cents_data$cent_type == cent_type, ]
  dist = vector("list", length(cbds[[cbd_by]]))
  names(dist) = cbds[[cbd_by]]
  for (cbd in cbds[[cbd_by]]) {
    zip_ids = which(zip_cents[[cbd_by]] == cbd)
    if (length(zip_ids) > 0) {
      dist[[cbd]] = data.frame(
        unique_plz = st_drop_geometry(zip_cents)[zip_ids, ]$unique_plz,
        cbd = cbd,
        dist = st_distance(zip_cents[zip_ids, ], cbds[cbds[[cbd_by]] == cbd, ])
      ) |> setNames(c("unique_plz", cbd_by, paste0("dist_", cent_type)))
    }
  }

  # bind list
  dist = data.table::rbindlist(dist, use.names = TRUE)
  dist
}


dists = lapply(unique(cents_lmrs_long$cent_type), \(v) calculate_dist(cents_lmrs_long, v))

# merge all distances and add original data for more zipcode info
dists = Reduce(\(x, y) merge(x, y, all = TRUE), dists)
dists = merge(st_drop_geometry(zip_cents), dists, all = TRUE)

data.table::setcolorder(dists, cbd_by, after = "amr_id")

## cbd = the most populous mun in the county ----
local({
  # municipality shapes
  muns = st_read("./data/geodata/admin-areas/municipalities.gpkg")
  muns = muns[, c("ags", "geo_name", "district_ags", "district_name", "geom")]
  # municipality pop
  pop = fread(
    "./data/processed/population_municipality.csv",
    select = c("ags", "pop_mun", "year"), keepLeadingZeros = TRUE
  ) |> subset(year == 2019, select = -3)
  # zipcode shapes
  zips = st_read("./data/geodata/zip-codes/unique_zipcodes_municipality.gpkg")
  zips = zips[, c("plz", "ags", "unique_plz", "geom")]

  # The CBD is the most populous municipality in the county.
  stopifnot(is.character(pop$ags))
  cbds = pop[, .(cbd = ags[which.max(pop_mun)]), .(did = substr(ags, 1, 5))]
  cbds = cbds[muns, on = c(cbd = "ags", did = "district_ags"), nomatch = NULL]
  setnames(cbds, "geo_name", "cbd_name")
  cbds = st_as_sf(cbds) |> st_centroid()

  # compute distance from the centroid of the zip code to the CBD
  dist2cbd = vector("list", length(cbds$did))
  names(dist2cbd) = cbds$did
  zips$did = substr(zips$ags, 1, 5)

  for (did in cbds$did) {
    cbd_shape = cbds[cbds$did == did, ]
    zips_in_the_county = zips[zips$did == did, ]
    if (nrow(zips_in_the_county) == 0) {
      warning("No zip codes in county ", did)
      next
    }
    dist2cbd[[did]] = data.frame(
      unique_plz = zips_in_the_county$unique_plz,
      cbd = cbd_shape$cbd,
      cbd_name = cbd_shape$cbd_name,
      did = did,
      dist_county = st_distance(zips_in_the_county, cbd_shape)
    )
  }

  dist2cbd = do.call(rbind, dist2cbd)
  dist2cbd
}) -> dist_county


## merge ----
dists = merge(
  dists, dist_county[, setdiff(names(dist_county), "did")], all = TRUE
)
setcolorder(dists, c("cbd", "cbd_name"), before = "dist_geo")

#--------------------------------------------------
# export

fwrite(dists, "./data/geodata/zip-codes/zip-codes_dist2cbd.csv")
