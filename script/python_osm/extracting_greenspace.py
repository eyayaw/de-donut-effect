#########################################################
# import libraries

import os
import osmnx
import folium
#from IPython.display import GeoJSON
import pandas as pd

#########################################################
# paths

mainPath = "H:/work/OSM-for-FTC/"
dataPath = mainPath + "data/"
outputPath = mainPath + "output/"

#########################################################
# set directory

os.chdir(mainPath)

#########################################################
# data extraction process

# set time stamp
settings = '[out:json][timeout:180][date:"{year}-12-31T00:00:00Z"]'

# define tags
greens = ["park", "garden", "nature_reserve"]

# folder structure (creates folders for storage)
for g in greens:
    isExist = os.path.exists(os.path.join(dataPath, g))
    if not isExist:
        os.makedirs(os.path.join(dataPath, g))

# define years
years = ["2017", "2018", "2019", "2020", "2021", "2022"]

# list to store
extracted_data = []

# loop through years and get snapshot at the time
for green in greens:
    for year in years:
        # define tag
        tag = {"leisure" : green}

        # set extraction year
        #osmnx.utils.config(overpass_settings=settings.format(year = year), timeout = 240)
        osmnx.settings.overpass_settings = settings.format(year = year)
        osmnx.settings.timeout = 300

        # extract data for tags and year
        tagged_data = osmnx.geometries_from_place("Germany", tags = tag)

        # add snapshot year
        tagged_data["snap:year"] = year

        # export data
        filename = str(green) + "_" + "extracted_data_" + str(year) + "_germany.csv"
        path = dataPath + str(green) + "/"
        tagged_data.to_csv(os.path.join(path, filename))

        # append list to store data
        extracted_data.append(tagged_data)

        # print "year" to see where the code is at
        print(f"Extraction of {year} and Germany for {tag} completed")