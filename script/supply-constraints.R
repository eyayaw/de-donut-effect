# -| load libraries -----
library(sf) # spatial features support
library(terra) # supersedes raster
library(ggplot2) # visualization workhorse
library(data.table)
# faster raster extraction
if (!requireNamespace("exactextractr")) {
  install.packages("exactextractr")
} else {
  library(exactextractr)
}

# -| helpers ----
# summary function
summ_f = function(x, w = rep(1, length(x)), na.rm = TRUE) {
  list(
    min = min(x, na.rm = na.rm),
    mean = weighted.mean(x, w = w, na.rm = na.rm),
    med = median(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    n = length(x) # number of pixel cells in each polygon i.e. district
  )
}

base.select <- `[` # base select for uses in pipes

# safely compare floating point numbers, taken from dplyr::near
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# set a unit of x to sqm
m2 <- function(x) units::set_units(x, m^2) # meter square (m^2)
unity <- function(x) units::set_units(x, 1) # unity (e.g. m^2/m^2 = 1 [1])


# -| import data ----
# import digital terrain model of Germany, resolution 200m
dtm = rast("data/geodata/dgm200.utm32s.gridascii/dgm200/dgm200_utm32s.asc")
zipcodes = st_read("data/geodata/zip-codes/unique_zipcodes_municipality.gpkg")[, c("unique_plz", "plz", "ags")]
st_geometry(zipcodes) = "geometry"
# selectors from the raster grid
zipcodes = st_transform(zipcodes, crs(dtm))
keys = c("unique_plz", "plz", "ags")
# raster extraction with geometric vector object: here polygons
## compute mean elevation at the grid-cell level from extracted raster cells

# -| elevation ----
elev = exact_extract(dtm, zipcodes, include_cols = keys)
elev_agg = do.call(rbind.data.frame, lapply(elev, \(x) cbind(x[1, keys], summ_f(x$value, x$coverage_fraction, na.rm = TRUE))))

elev_agg$range = sapply(elev, \(x) diff(range(x$value, na.rm = TRUE)))
elev_agg$wn = sapply(elev, \(x) sum(x$coverage_fraction)) # weighted num of cells


fwrite(elev_agg, "data/geodata/constraints/elevation_zipcodes.csv")

# -| TRI ----
# Compute TRI terrain ruggedness index from elevation data
# https://www.usna.edu/Users/oceano/pguth/md_help/html/topo_rugged_index.htm
# TRI (Terrain Ruggedness Index) is the mean of the absolute differences between the value of a cell and the value of its 8 surrounding cells (Riley et al. (1999))


## -| Riley et al. (1999) -----
## see https://www.researchgate.net/publication/259011943
tri_riley = focal(dtm,
  w = matrix(1, nrow = 3, ncol = 3),
  fun = function(x) sqrt(sum((x[-5] - x[5])**2))
)

# TRI values need to be averaged over grid cells for a TRI at the grid level
tri_riley_agg = exact_extract(tri_riley, zipcodes, include_cols = keys)
tri_riley_agg = tri_riley_agg |>
  (\(d) data.frame(
    do.call(rbind.data.frame,  Map(\(x) cbind(x[1, keys], summ_f(x$value, x$coverage_fraction, TRUE)), d)),
    wn = sapply(d, \(x) sum(x$coverage_fraction)) # weighted num of cells
  ))()


# classification: using "equal area" classification method to group continuous
# ranges of TRI values into seven classes of unequal range, but equal area.

riley_cats = "range;label
0-80;level
81-116;nearly level
117-161;slightly rugged
162-239;intermediately rugged
240-497;moderately rugged
498-958;highly rugged
959-Inf;extremely rugged"

riley_cats = read.csv(text = riley_cats, sep = ";", header = TRUE)
riley_cats$upper = as.numeric(sub("(.*)-(.+)$", "\\2", riley_cats$range))
tri_riley_agg$tri_cat =
  cut(tri_riley_agg$mean, breaks = c(0, riley_cats$upper), label = riley_cats$label)

# show tri heterogeneity across space
tri_riley_agg = merge(zipcodes, tri_riley_agg, keys)

if (FALSE) {
  ggplot(tri_riley_agg, aes(fill = tri_cat)) +
    geom_sf(lwd = 0, color = NA) +
    scale_fill_viridis_d("Categories", direction = -1L) +
    labs(title = "Terrain Ruggedness Index") +
    theme_void()
}

# custom classification
if (FALSE) {
  tri_riley_agg |>
    transform(tri = cut(mean, 7)) |>
    ggplot(aes(fill = tri)) +
    geom_sf(lwd = 0, color = NA) +
    scale_fill_viridis_d("TRI", direction = -1L) +
    theme_void()
}

fwrite(st_drop_geometry(tri_riley_agg), "data/geodata/constraints/TRI_zipcodes.csv")

# -| compute fractions of developed and undevelopable land ----
## extract the exact fraction of land that is not developable
## undevelopable area =  built-up, water, wetlands, and slope > 15%

# get factor id (in categorical raster r) given the clc code
get_id <- function(r, code) {
  Cats = cats(r)[[1]]
  Cats[match(as.integer(code), as.integer(Cats$code)), ]$ID
}

# inverse of get_id
get_code <- function(r, id) {
  Cats = cats(r)[[1]]
  Cats[match(as.integer(id), as.integer(Cats$ID)), ]$code
}

## convert radians to slope percent
## https://geogra.uah.es/patxi/gisweb/DEMModule/DEM_T_Sl.htm
rad2perc = \(r) tan(r) * 100
rad2deg = \(r) 180 * r / pi
deg2rad = \(d) (d * pi) / 180

pix_area = m2(prod(res(dtm))) # area of a pixel (in m^2), res (200x200)
crs_dtm = terra::crs(dtm, proj = TRUE)

# aggregator function
agg.f = function(val, cfrac) {
  masked = near(val, maskval_perc)
  nan = is.nan(val)
  list(
    # weighted num pixels
    # wnpix = sum(cfrac, na.rm = TRUE), # pixel_area * wnpix == admin_area of district

    ## stock of land avail for development: agri + forest + (artificial if not a masker)
    # pixel_area * wn, wn = weighted number of cells, excludes cells w/ NaN slope
    area_avail = pix_area * sum(cfrac[!(nan | masked)]),

    # pixel_area * sum of weighted fraction of the masked cells, potentially, equals
    # [artificial + wet + water] or [wet + water] => (the masked part above)
    area_masked = pix_area * sum(cfrac[masked], na.rm = TRUE),

    # weighted NA|NaN count
    area_nan = pix_area * sum(cfrac[nan], na.rm = TRUE),

    # pixel_area * count of cells with slope > 15 %
    area_gr_15 = pix_area * sum(cfrac * (val > 15), na.rm = TRUE)
  )
}


## -| Corine land cover classes -----
# 37 classes at disaggregated, 5 classes at aggregated level
clc_classes = fread("data/geodata/CLC/clc-classification.csv") |>
  base.select(j = c("clc_code", "classname", "classname3", "cols1", "cols3"))
clc_classes = cbind(clc_classes, t(col2rgb(clc_classes$cols1, TRUE)))
fpath = "clc-2018/U2018_CLC2018_V2020_20u1.tif"

## -| compute the fraction of area with slope > 15 % ----
## -| computation ----
### -| steps
### -| step 0: compute slope ----
slope = terrain(dtm, "slope", unit = "radians", neighbors = 8)

### -| step 1: construct maskers -----
clc = rast(sprintf("data/geodata/CLC/%s", fpath))
if (crs(clc, proj = TRUE) != crs_dtm) {
  clc <- project(clc, dtm, mask = TRUE)
}

Cats = cats(clc)[[1]] # categories of the raster (factor) values
Cats = Cats[!is.na(Cats[, grep("^code_", names(Cats), value = TRUE)]), ]


## area covered by land cover classes classes
# extract values by keys
clc_extract = exact_extract(clc, zipcodes, include_cols = keys) |>
  rbindlist(use.names = TRUE)

clc_extract[, code := as.integer(get_code(clc, value))]

clc_extract = clc_extract[,
  .(area = sum(coverage_fraction) * pix_area),
  c(keys, "code")
]

fwrite(
  clc_extract,
  sprintf("data/geodata/constraints/land-cover_area_%s_by-clc-class1.csv", substr(fpath, 1, 8))
)
# aggregate by id and land cover class 3 (higher level)
clc_extract[, code3 := code %/% 100]
clc_extract_agg = clc_extract[, .(area = sum(area)), c(keys, "code3")]
clc_extract_agg = dcast(clc_extract_agg, as.formula(paste(paste(keys, collapse = "+"), "~ code3")), value.var = "area")
setnames(clc_extract_agg, c("NA", 1:5), c("n.a", "artificial", "agri", "forest", "wetlands", "water"))


## separate some land cover classes
maskers = c(`artificial-wetlands-water` = "^(1|4|5)", `wetlands-water` = "^(4|5)")

for (j in seq_along(maskers)) {
  fac_ids = get_id(clc, subset(clc_classes, grepl(maskers[[j]], clc_code))$clc_code)
  m = with(Cats, cbind(ID, ifelse(ID %in% fac_ids, ID, NA)))
  clc_masker = classify(clc, m)

  ### -| step 3: clip or mask the slope raster by the masker raster ----

  # first, keep the slope raster aligning with the grid,
  # then, remove polygons that are masked (given by the selected masker classes)

  # replace the slope value of the masked cells with
  maskval_rad = -pi / 4 # rads (-45 degree) -> slope -100
  maskval_perc = rad2perc(maskval_rad)

  # mask by selected masker classes above and keep the unmasked part
  slope_clip = mask(slope, clc_masker, updatevalue = maskval_rad, inverse = TRUE)
  values(slope_clip) = rad2perc(values(slope_clip)) # slope_% = tan(slope_rad)*100

  ### -| step 4: aggregate pixel level slope to the chosen spatial scale level ----
  ## e.g. grid-cell, zipcode, district

  # This involves extracting slope values from the raster that are covered by the
  # grid-cell shapes: using a summary function (e.g. mean) then, we can aggregate
  # pixel level slope values to the desired level.
  #
  # Detailed steps:
  # First, extract the area covered by each grid-cell in the slope raster.
  # This area of the grid-cell excludes the area covered with (1) wet land,
  # (2) water bodies, and (3) artificial surfaces, depending on the selected maskers.
  # We achieve this by masking cells in the slope raster with Corine Land Cover
  # raster with these three categories.
  # Next, for each grid-cell, we find the sum of the area that exhibits slope greater
  # than 15%--out of the area that is developable (i.e., not masked).

  # Note that we define the total developable stock as the area of the grid-cell
  # that is not covered by wetlands and water bodies(, and artificial surfaces).
  # We are interested in computing what fraction of this quantity exhibits
  # slope greater than 15%.
  # Then, the ratio of (the sum of wetlands, water, and 15%>) =: total undevelopable land
  # to total land gives us the fraction of land that is undevelopable.
  #

  slope_extract = exact_extract(slope_clip, zipcodes, include_cols = keys)
  # bind data frames, the above returns a list of data frames one for each unit
  slope_extract = rbindlist(slope_extract, use.names = TRUE)
  setnames(slope_extract, c("value", "coverage_fraction"), c("val", "cfrac"))

  # min, mean, med, max, sd, n
  slope_agg = slope_extract[
    !(near(val, maskval_perc) | is.nan(val)),
    .(slope_mean = weighted.mean(val, cfrac), slope_range = diff(range(val))),
    c(keys)
  ]
  slope_agg = merge(slope_agg, slope_extract[, agg.f(val, cfrac), keys], keys)
  slope_agg[, area := rowSums(cbind(area_avail, area_masked, area_nan), na.rm = TRUE)]

  ### -| step 5: compute undevelopable fraction ----

  # what fraction of the total area of a district is undevelopable?
  constraints = merge(slope_agg, clc_extract_agg, keys)
  # undevelopable fraction according to our definition above
  constraints[, unavail_frac := (1 / area) * rowSums(cbind(area_gr_15, area_masked), na.rm = TRUE)]

  # check if undevelopable fraction is above 1, it should not be, of course!
  stopifnot(max(constraints$unavail_frac) <= 1L)

  fwrite(constraints,
    sprintf("data/geodata/constraints/constraints_%s_masked_zipcodes.csv", names(maskers)[[j]])
  )
}
