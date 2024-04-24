source("./script/create_admin-areas.R")
source("./script/create_zip-codes.R")

# does extraction of the original RED data, that comes as a zip file
# source("./script/hedonic-model/prepare_prices-rents_data.R")
source("./script/hedonic-model/prepare_prices-rents_data_no-extraction.R")

source("./script/hedonic-model/clean_prices.R")
source("./script/hedonic-model/clean_rents.R")

source("./script/clean_microm.R") # population data for weighting cbds
source("./script/clean_population-municipality.R")

source("./script/make_unique_zipcodes.R")
source("./script/crosswalking.R")
source("./script/create_centroids_muns.R")
source("./script/create_centroids_lmrs.R")

source("./script/compute_dist2cbd_grids.R")
source("./script/compute_dist2cbd_zipcodes.R")

source("./script/hedonic-model/hedonic-model_prices.R")
source("./script/hedonic-model/hedonic-model_rents.R")

source("./script/merge_datasets.R")
source("./script/bidrent.R")
