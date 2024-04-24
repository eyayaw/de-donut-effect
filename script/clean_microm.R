library(data.table)
since = 2018 # keep data onward

## controls ----
# list of important variables that could be used as controls
nms = c(
  grid_id = "r1_id",
  year = "year",
  inhabitants = "r1_ewa_a_gesamt",
  num_hhs = "r1_mba_a_haushalt",
  resid_buildings = "r1_mba_a_wohngeb",
  purchase_power = "r1_kkr_w_summe",
  unemp = "r1_alq_p_quote",
  car_density = "r1_mpi_w_dichte",
  share_background_german = "r1_met_p_deutschl",
  share_male_1820 = "r1_eag_p_m18bis20",
  share_male_2025 = "r1_eag_p_m20bis25",
  share_male_2530 = "r1_eag_p_m25bis30",
  share_male_3035 = "r1_eag_p_m30bis35",
  share_male_3540 = "r1_eag_p_m35bis40",
  share_male_4045 = "r1_eag_p_m40bis45"
)

microm = fread(
  "../Common-Data/.GRID_v11/csv/GRID_sufv11.csv",
  select = unname(nms), na.strings = "-1"
)
setnames(microm, nms, names(nms))

microm = microm[!(grid_id %ilike% "xxx"), ]
# complete cases
all_combinations = CJ(grid_id = microm$grid_id, year = microm$year, unique = TRUE)
microm = microm[all_combinations, on = .(grid_id, year), ]
# filling missing values
microm = microm[, lapply(.SD, nafill, type = "locf"), by = grid_id] # year is ignored b/c it's completed by CJ
microm = microm[, lapply(.SD, nafill, type = "nocb"), by = grid_id]

# impute from each other
microm[is.na(resid_buildings), resid_buildings := num_hhs]
microm[is.na(num_hhs), num_hhs := resid_buildings]
microm[is.na(inhabitants), inhabitants := num_hhs * 2] # 2 here is ave household size

microm[, share_male_1845 := rowSums(.SD, na.rm = TRUE),
       .SDcols = grep("^share_male", names(microm), value = TRUE)
]

microm = microm[, !(share_male_1820:share_male_4045)]
microm = microm[year >= (since), ]

fwrite(microm, "data/processed/microm-tidy.csv")
