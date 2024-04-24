library(ggplot2)
library(sf)
library(data.table)

## grided ----
grid_admin = fread("../Common-Data/.GRID_v11/Zuordnung_Gemeinden/2020_Grids_Municipality_Exact_unambiguous.csv", keepLeadingZeros = TRUE)
setnames(grid_admin, c("r1_id", "AGS", "share"), c("grid_id", "ags", "grid_area_share_in_ags"))

hpi_grid = fread('data/processed/final_HPI_grid_all-homes.csv', keepLeadingZeros = TRUE)
hri_grid = fread('data/processed/final_HRI_grid_flats.csv', keepLeadingZeros = TRUE)

hpi_grid[, `:=`(ngrids_ymg = .N), .(group, mon, year)]
hri_grid[, `:=`(Ngrids_ymg = .N), .(group, mon, year)]

## population weighted average price/rental indexes by group
hpi_bucket = hpi_grid[rank_lmr<=15,
  .(val = weighted.mean(lnhpi, inhabitants/nads, na.rm = TRUE), .N),
  .(year, mon, group)
  ][, `:=`(index = val / val[year == 2020 & mon == "01"]), group]

# hpi_bucket[, quart := floor(1 + as.integer(mon)/4)]
# hpi_bucket = hpi_bucket[, .(lnhpi = mean(lnhpi)), .(group, year, quart)]
# hpi_bucket[, `:=`(index = lnhpi/lnhpi[year == 2019 & quart == 4]), group]


# stl decomposition
hpi_stl = hpi_bucket[order(group, year, mon), .(group, mon, year, val)] |>
  split(by = "group") |>
  lapply(\(x) x[, !"group"]) |>
  lapply(\(x) ts(x[, val], frequency = 12, start = c(2018, 1))) |>
  lapply(stl, s.window = "periodic") |>
  lapply(\(x) as.data.frame(x$time.series)) |>
  rbindlist(idcol = 'group')

hpi_stl = cbind(hpi_stl, CJ(year=2018:2021, mon=sprintf("%02i", 1:12), sorted = FALSE))

hpi_stl = hpi_stl[hpi_bucket, on=c('group', 'year', 'mon')
                  ][,`:=`(val0=val-seasonal)
                    ][, `:=`(index0 = val0 / val0[year == 2020 & mon == "02"]), group]

## rental index
hri_bucket = hri_grid[rank_lmr<=15,
.(val = weighted.mean(lnhri, inhabitants/nads, na.rm = TRUE)),
.(year, mon, group)
][, `:=`(index = val / val[year == 2020 & mon == "01"]), group]


# stl decomposition
hri_stl = hri_bucket[order(group, year, mon), .(group, mon, year, val)] |>
  split(by = "group") |>
  lapply(\(x) x[, !"group"]) |>
  lapply(\(x) ts(x[, val], frequency = 12, start = c(2018, 1))) |>
  lapply(stl, s.window = "periodic") |>
  lapply(\(x) as.data.frame(x$time.series)) |>
  rbindlist(idcol = 'group')

hri_stl = cbind(hri_stl, CJ(year=2018:2021, mon=sprintf("%02i", 1:12), sorted = FALSE))

hri_stl = hri_stl[hri_bucket, on=c('group', 'year', 'mon')
][,`:=`(val0=val-seasonal)
][, `:=`(index0 = val0 / val0[year == 2020 & mon == "02"]), group]



# visualization ----
hri_stl |>
  ggplot() +
  geom_line(aes(as.Date(paste0(year, "-",mon, "-01")), index0, color = group), lwd = 1.1) +
  labs(x = NULL, color = NULL) +
  geom_vline(xintercept = as.Date(paste0("2020-", "02", "-01"))) +
  scale_color_manual(values = setNames(
    c("black", "forestgreen", "darkred", "darkblue"),
    c("low", "mid", "high", "cbd")
  )) +
  theme_classic(14) +
  theme(legend.position = c(.7, .2)) +
  guides(color = guide_legend(direction = "horizontal"))


## maps ----
grids = st_read("../Common-Data/.GRID_v11/Raster_shp/ger_1km_rectangle.shp")[, c("idm", "geometry")]
names(grids)[1] = "grid_id"
lmrs = st_read("data/geodata/LMR/2013_12_AMR_NEU.shp") |> st_transform(st_crs(grids))
names(lmrs) = tolower(names(lmrs))
lmrs = lmrs[, c("amr_id", "amr_name", "geometry")]

cbds = st_read("data/geodata/CBDs/CBD_AMR_EK_COM.gpkg") |> st_transform(st_crs(lmrs))
names(cbds) = tolower(names(cbds))
names(cbds)[1] = "amr_id"

lmrs_grid = fread("data/labor-market-regions/grid-germany-with-lmrs.csv", select = c("r1_id", "AMR_ID", "AMR_name"))
setnames(lmrs_grid, tolower)
setnames(lmrs_grid, "r1_id", "grid_id")

# cuts out the grid cells outside the boundary of Germany
grids = merge(grids, lmrs_grid, "grid_id")

hpi_sf = hpi_grid[, .(lnhpi = mean(lnhpi, na.rm = TRUE)), .(grid_id, year)]
hpi_sf = hpi_sf[grids, on = "grid_id", nomatch = NULL]
hri_sf = hri_grid[grids, on = "grid_id", nomatch = NULL]
hpi_sf |>
  subset(amr_id == 109 & year %in% c(2020, 2021)) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(data = lmrs[lmrs$amr_id == 109, ], fill = NA, lwd = 0.05) +
  geom_sf(aes(fill = lnhpi), color = NA, alpha = 0.5) +
  facet_wrap(~year, ncol = 2) +
  scale_fill_viridis_c(direction = -1L) +
  guides(fill = guide_colorbar(direction = "horizontal")) +
  #geom_sf(data = cbds, size = .5, color = "red") +
  theme_void() +
  theme(legend.position = "top")


# labor market regions
ggplot(lmrs) +
  geom_sf(fill = NA, lwd = .25, show.legend = FALSE) +
  geom_sf_text(aes(label = (amr_name)), size = 3) +
  theme_void()


# estimation ----
hpi[mon %in% c("01", "02", "03"), ][
  order(grid_id, year, mon),
  .(year, lnhpi, pct_price = 100 * exp(lnhpi) / shift(exp(lnhpi)) - 100),
  .(grid_id, mon)
]
