library(data.table)
library(sf)
library(ggplot2)

source('script/helpers/helpers.R')

ref_mon = 3L
ref_year = 2020L
ref_time = as.Date(sprintf("%s-%02i-01", ref_year, ref_mon))
end_year = 2021L

## maps --------------
hedonic = fread("data/processed/final_indexes_grid.csv",
  select = c("grid_id", "ags", "did", "amr_id", "var", "time", "year", "mon", "val"),
  keepLeadingZeros = TRUE
)

de_grid = st_read("data/processed/geodata/grid-germany.gpkg")
de_grid = de_grid[, c('grid_id', 'ags', 'geo_name', 'geom')]


grid_LMRs = st_read("data/geodata/labor-market-regions/2013_12_AMR_NEU.shp") |>
  st_transform(st_crs(de_grid))

grid_LMRs = grid_LMRs[, c("AMR_ID", "AMR_name", 'geometry')]
names(grid_LMRs) = tolower(names(grid_LMRs))

amrs = st_read(
  "data/geodata/labor-market-regions/AMR_EK/SHAPEFILE/"
)[, c("RAM_Name", "RAM_2016")] |>
  setNames(c('amr_name', 'amr_id'))

LMRs = fread("data/geodata/labor-market-regions/AMR_EK/labor-market-regions.csv",
  col.names = c("amr_name", "kreise")
)

LMRs = cbind(amr_name = LMRs$amr_name, LMRs[, tstrsplit(kreise, ",")]) |>
  melt(id.vars = 'amr_name', measure=patterns("^V")) |>
  subset(!is.na(value))

LMRs[, c("did", "name") := .(
  sub("(\\d{4,5}).+", "\\1", value),
  sub("(\\d{4,5} )(.+)", "\\2", value)
)][, `:=`(variable = NULL, value = NULL)
   ][, did := sprintf("%05i", as.integer(did))]

# LMRs[!amrs, on='amr_name'] # not matching

amrs = merge(amrs, LMRs)


# cbds = st_read("data/geodata/CBDs/CBD_AMR_EK_COM.gpkg") |> st_transform(st_crs(de_grid))
# st_geometry(cbds) = "geometry"
# names(cbds)[match("LLM_EK", names(cbds))] = "amr_id"

# cbds = merge(cbds, amrs, "amr_id")


grid_LMRs = merge(grid_LMRs, amrs[, c("amr_id", "did")], "amr_id")

hedonic_geo = merge(de_grid, hedonic, c('grid_id', 'ags'))

selected = c(
  berlin = "11000",
  hamburg = "02000",
  munich = "09184",
  frankfurt = "06412",
  cologne = "05315",
  bremen = "04011",
  dortmund = "05913",
  essen = "05113",
  duisburg = "05112"
)

measures = c("lnhri", "lnhpi")
out = vector("list", length(selected))
names(out) = names(selected)

for (measure in measures) {
  for (i in seq_along(selected)) {
  hedonic_geo |>
    subset(var == (measure) & did == selected[[i]] &
           year %in% c(ref_year, end_year) & mon == ref_mon,
           select = c('val', "year")
    ) -> d

  out[[i]] = ggplot() +
    geom_sf(aes(fill = val), data = d, lwd=.25) +
    scale_fill_viridis_c(
      direction = -1L,
      guide = guide_colorbar(
        # direction = "vertical",
        # barheight = unit(.5, "cm"),
        barwidth = unit(.55, "cm"),
        label.theme = element_text(size = 9),
        # label.vjust = 2,
        title.position = "top",
        # title.vjust = .75,
        title.theme = element_text(size = 11)
      )
    ) +
    facet_grid(~year) +
    geom_sf(fill = NA, lwd = 0.5, color = "blue", data = grid_LMRs[grid_LMRs$did == selected[[i]], ]) +
    geom_sf(fill = NA, lwd = 2.5, color = "red", data = st_centroid(grid_LMRs[grid_LMRs$did == selected[[i]], "geometry"])) +
    labs(
      title = sprintf(
        "%s in %s (%s)",
        getLabel(measure), month.name[ref_mon], tools::toTitleCase(names(selected)[i])
      ),
      fill = getLabel(measure, math = TRUE)
    ) +
    cowplot::theme_map() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    )
}

for (i in seq_along(selected)) {
  ggsave(sprintf("doc/figs/%s_%s-grid.png", names(selected)[[i]], measure), out[[i]], width = 8, height = 5)
}
}
