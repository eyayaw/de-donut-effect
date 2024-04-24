library(sf)
source("script/helpers/helpers.R")

## import shapes
zipcodes = st_read("./data/geodata/zip-codes/unique_zipcodes_municipality.gpkg")
lmrs = st_read("./data/geodata/labor-market-regions/Labor-Market-Regions_Kosfeld-Werner-2012_2019.gpkg")
grids = st_read("./data/geodata/germany-grid/de-grid.gpkg")


## cleaning
# transform all other projections into
main_crs = st_crs(zipcodes)
lmrs = st_transform(lmrs, st_crs(zipcodes))
grids = st_transform(grids, main_crs)

## crosswalking zipcode to labor market region based on its county ----
# zip codes to labor market regions (cbds)
zipcodes$did = substr(padd_zero(zipcodes$ags), 1, 5)

xwalk_zipcodes_lmrs = vector("list", length(zipcodes$unique_plz))
county_list = strsplit(lmrs$district_id, "[|]")
for (z in seq_along(xwalk_zipcodes_lmrs)) {
  # get the lmr of the zipcode based on its county
  lmr_df = st_drop_geometry(lmrs)[Position(\(x) zipcodes$did[[z]] %in% x, county_list), ]
  lmr_df$unique_plz = zipcodes$unique_plz[[z]]
  xwalk_zipcodes_lmrs[[z]] = lmr_df[, c("unique_plz", "amr_id", "amr_name")]
}

xwalk_zipcodes_lmrs = data.table::rbindlist(xwalk_zipcodes_lmrs)
xwalk_zipcodes_lmrs = merge(zipcodes, xwalk_zipcodes_lmrs) # add shape of the zipcode

## crosswalking grids to zip codes ----
xwalk_grids_zipcodes = st_join(grids, zipcodes, join = st_intersects, largest = TRUE)

## crosswalk grids to labor market regions
xwalk_grids_lmrs = st_join(grids, lmrs, join = st_intersects, largest = TRUE)


# write ----

st_write(
  xwalk_zipcodes_lmrs, "./data/geodata/zip-codes/xwalk_zipcodes-lmrs.gpkg", append = FALSE
)
data.table::fwrite(
  st_drop_geometry(xwalk_zipcodes_lmrs), "./data/geodata/zip-codes/xwalk_zipcodes-lmrs.csv"
)


st_write(
  xwalk_grids_zipcodes, "./data/geodata/zip-codes/xwalk_grids-zipcodes-muns.gpkg", append = FALSE
)
data.table::fwrite(
  st_drop_geometry(xwalk_grids_zipcodes), "./data/geodata/zip-codes/xwalk_grids-zipcodes-muns.csv"
)


st_write(
  xwalk_grids_lmrs, "./data/geodata/xwalk_grids-lmrs.gpkg", append = FALSE
)
data.table::fwrite(
  st_drop_geometry(xwalk_grids_lmrs), "./data/geodata/xwalk_grids-lmrs.csv"
)
