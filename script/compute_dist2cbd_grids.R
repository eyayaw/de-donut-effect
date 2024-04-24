library(data.table)
library(sf)


# Grided Germany
de_grid = st_read("data/geodata/xwalk_grids-lmrs.gpkg")

# the cbds:
# coordinates of centroids (geo + weighted)
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

cents_lmrs_long = st_as_sf(cents_lmrs_long, coords = c("lon", "lat"), crs = st_crs(de_grid))

## distance to weighted cbds ----
cbd_by = "amr_name"

calculate_dist <- function(x, y, keep_vars = NULL) {
  if (is.null(keep_vars)) {
    keep_vars = names(x)
  }

  dist = vector("list", length(y[[cbd_by]]))
  names(dist) = y[[cbd_by]]

  for (i in seq_along(dist)) {
    from_ids = which(x[[cbd_by]] == y[[cbd_by]][[i]])
    if (length(from_ids) > 0) {
      dist[[i]] = data.frame(
        st_drop_geometry(x[from_ids, keep_vars]),
        dist = st_distance(x[from_ids, ], y[y[[cbd_by]] == y[[cbd_by]][[i]], ])
      )
    }
  }

  # bind list
  dist = data.table::rbindlist(dist, use.names = TRUE)
  dist
}

cent_types = unique(cents_lmrs_long$cent_type)
dists = lapply(
  cent_types,\(v) calculate_dist(de_grid, subset(cents_lmrs_long, cent_type == v), "grid_id") |>
    {\(.x) setNames(.x, c(names(.x)[-ncol(.x)], paste0("dist_", v)))}()
)

# merge all distances and add original data for more zipcode info
dists = Reduce(\(x, y) merge(x, y, all = TRUE), dists)
dists = merge(st_drop_geometry(de_grid), dists, all = TRUE)


# the cbd being the most populous municipality in the county
local({
  # municipality shapes
  muns = st_read("./data/geodata/admin-areas/municipalities.gpkg")
  muns = muns[, c("ags", "geo_name", "district_ags", "district_name", "geom")]
  # municipality pop
  pop = fread(
    "./data/processed/population_municipality.csv",
    select = c("ags", "pop_mun", "year"), keepLeadingZeros = TRUE
  ) |> subset(year == 2019, select = -3)
  # grid shapes
  de_grid = st_read("data/geodata//admin-areas/grid-germany_with-admin-areas.gpkg")
  de_grid = de_grid[, c("grid_id", "ags")]

  # The CBD is the most populous municipality in the county.
  stopifnot(is.character(pop$ags))
  cbds = pop[, .(cbd = ags[which.max(pop_mun)]), .(did = substr(ags, 1, 5))]
  cbds = cbds[muns, on = c(cbd = "ags", did = "district_ags"), nomatch = NULL]
  setnames(cbds, "geo_name", "cbd_name")
  cbds = st_as_sf(cbds) |> st_centroid()

  # compute distance from the centroid of the zip code to the CBD
  dist2cbd = vector("list", length(cbds$did))
  names(dist2cbd) = cbds$did
  de_grid$did = substr(de_grid$ags, 1, 5)

  for (did in cbds$did) {
    cbd_shape = cbds[cbds$did == did, ]
    grids_in_the_county = de_grid[de_grid$did == did, ]
    if (nrow(grids_in_the_county) == 0) {
      warning("No grids in county ", did)
      next
    }
    dist2cbd[[did]] = data.frame(
      grid_id = grids_in_the_county$grid_id,
      cbd = cbd_shape$cbd,
      cbd_name = cbd_shape$cbd_name,
      did = did,
      dist_county = st_distance(grids_in_the_county, cbd_shape)
    )
  }

  dist2cbd = do.call(rbind, dist2cbd)
  dist2cbd
}) -> dist_county

dists = merge(dists, dist_county, "grid_id")
setcolorder(dists, c("cbd", "cbd_name", "did"), before = "dist_geo")

fwrite(dists, "./data/geodata/grids_dist2cbd.csv")
