# --------------------------------------------------
# import libraries

import pandas as pd
from os.path import join
import glob
import geopandas as gpd
from config import configurations

# --------------------------------------------------
# paths

data_path = configurations().get("data_path")

# --------------------------------------------------
# set up

# define tags
greens = ["park", "garden", "nature_reserve"]

# define columns you want to keep
cols = ["element_type", "osmid", "name", "snap:year", "geometry"]

# --------------------------------------------------
# read and clean function
def clean_fun(filename):

    # keep only key columns
    file = filename[cols]

    # drop in case of "relation" and "node"
    file = file.loc[(file["element_type"] != "relation")]
    #file = file.loc[(file["element_type"] != "node")]

    # set as spatial data
    file["geometry"] = gpd.GeoSeries.from_wkt(file["geometry"])
    file_spatial = gpd.GeoDataFrame(file, geometry = "geometry", crs = "EPSG:4326")
    file_spatial = file_spatial.to_crs(32632)

    # return output
    return file_spatial

# --------------------------------------------------
# loop through places and apply function

# define years to loop through
years = ["2017", "2018", "2019", "2020", "2021", "2022"]

# loop
for green in greens:
    # store data for all years and one greenspace at the time
    all_files = []
    for year in years:
        # get files for greenspaces (i.e. each city and year)
        path = join(data_path, green)
        exp = "*_" + str(year) + "_germany.csv"
        files = glob.glob(join(path, exp))
        for file in files:
            # read in file
            data = pd.read_csv(file, index_col = None, header = 0, low_memory = False)
            # apply clean function
            data = clean_fun(filename = data)
            # store all files in one list
            all_files.append(data)
            
    # turn list into data frame
    complete_data = pd.concat(all_files, axis = 0, ignore_index = True)

    # renaming
    complete_data.rename(columns = {"snap:year" : "year", "element_type" : "geo_type"}, inplace = True)

    # add source
    complete_data["source"] = str(green)

    # save data frame
    filename = green + "_germany_all_years.csv"
    complete_data.to_csv(join(path, filename), index = False)

    # save as shape file
    complete_data_spatial = gpd.GeoDataFrame(complete_data, geometry = "geometry", crs = 32632)

    # check if geometry == Polygon otherwise with mixed type it cannot be save (but just a few are
    # non-Polygon)
    for index, row in complete_data_spatial.iterrows():
        if row["geometry"].geom_type != "Polygon":
            complete_data_spatial = complete_data_spatial.drop(index)

    filename_shp = green + "_germany_all_years.shp"
    complete_data_spatial.to_file(filename = join(path, filename_shp), index = False)

    # print "year" to see where the code is at
    print(f"Extraction of {year} and {green} completed")