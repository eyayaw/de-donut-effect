#########################################################
# import libraries

import os
import osmnx
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
food = ["restaurant", "bar", "cafe", "pub", "ice_cream"]
entertainment = ["cinema", "theatre"]
education = ["college", "university", "kindergarten", "school", "library"]

places = food + entertainment + education

# folder structure (creates folders for storage)
for p in places:
    isExist = os.path.exists(os.path.join(dataPath, p))
    if not isExist:
        os.makedirs(os.path.join(dataPath, p))

# define years
years = ["2017", "2018", "2019", "2020", "2021", "2022"]

# list to store
extracted_data = []

# loop through years and get snapshot at the time
for place in places:
    for year in years:
        # define tag
        tag = {"amenity" : place}

        # set extraction year
        #osmnx.utils.config(overpass_settings=settings.format(year = year), timeout = 240)
        osmnx.settings.overpass_settings = settings.format(year = year)
        osmnx.settings.timeout = 300

        # extract data for tags and year
        tagged_data = osmnx.geometries_from_place("Germany", tags = tag)

        # add snapshot year
        tagged_data["snap:year"] = year

        # export data
        filename = str(place) + "_" + "extracted_data_" + str(year) + "_germany.csv"
        path = dataPath + str(place) + "/"
        tagged_data.to_csv(os.path.join(path, filename))

        # append list to store data
        extracted_data.append(tagged_data)

        # print "year" to see where the code is at
        print(f"Extraction of {year} and Germany for {tag} completed")
