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

# define places
food = ["restaurant", "bar", "cafe", "pub", "ice_cream"]
entertainment = ["cinema", "theatre"]
education = ["college", "university", "kindergarten", "school", "library"]

places = food + entertainment + education

# define columns you want to keep (for each place)
cols = ["element_type", "osmid", "amenity", "name", "snap:year", "geometry"]

# --------------------------------------------------

# read and clean function
def read_and_clean(filename, path):
    # read in file
    file = pd.read_csv(filename, index_col = None, header = 0, low_memory = False)

    # select needed columns
    file = file.reindex(columns = cols)

    # drop in case of "relation"
    file = file.loc[(file["element_type"] != "relation")]

    # set as spatial data
    file["geometry"] = gpd.GeoSeries.from_wkt(file["geometry"])
    file_spatial = gpd.GeoDataFrame(file, geometry = "geometry", crs = "EPSG:4326")
    file_spatial = file_spatial.to_crs(32632)

    # replace ways (i.e. those that have the building as geo info) with centroid of building
    file_spatial.loc[(file_spatial.element_type == "way"), "geometry"] = file_spatial["geometry"].centroid

    # change element_type to indicate whether geometry is given or calculated
    file_spatial.loc[(file_spatial.element_type == "node"), "element_type"] = "given"
    file_spatial.loc[(file_spatial.element_type == "way"), "element_type"] = "calculated"

    return file_spatial

# --------------------------------------------------
# loop through places and apply function

years = ["2017", "2018", "2019", "2020", "2021", "2022"]

for place in places:
    # store data for all years and one place at the time
    all_files = []
    for year in years:
        # get files for places (i.e. each city and year)
        path = join(data_path, place)
        exp = "*_" + str(year) + "_germany.csv"
        files = glob.glob(join(path, exp))
        for file in files:
            # apply read and clean function to each file
            data = read_and_clean(filename = file, path = path)

            # store all files in one list
            all_files.append(data)
            
        # turn list into data frame
        complete_data = pd.concat(all_files, axis = 0, ignore_index = True)

        # renaming
        complete_data.rename(columns = {"snap:year" : "year", "element_type" : "geo_type"}, inplace = True)

        # add source
        complete_data["source"] = str(place)
                
        # save data frame
        filename = place + "_germany_all_years.csv"
        complete_data.to_csv(join(path, filename), index = False)

        # save as shape file
        complete_data_spatial = gpd.GeoDataFrame(complete_data, geometry = "geometry", crs = 32632)
        filename_shp = place + "_germany_all_years.shp"
        complete_data_spatial.to_file(filename = join(path, filename_shp), index = False)

        # print "year" to see where the code is at
        print(f"Extraction of {year} and {place} completed")