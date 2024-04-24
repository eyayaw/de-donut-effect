# --------------------------------------------------
# import libraries

import os
import osmnx
import pandas as pd
from os.path import join

# --------------------------------------------------
# paths

main_path = "H:/work/OSM-for-FTC/"
data_path = main_path + "data/"

# --------------------------------------------------
# data extraction process

# set time stamp
year = 2019
settings = '[out:json][timeout:180][date:"{year}-12-31T00:00:00Z"]'

# define tags
place = "townhall"

# define tag
tag = {"amenity" : place}

# set extraction year
osmnx.settings.overpass_settings = settings.format(year = year)
osmnx.settings.timeout = 300

# extract data for tags and year
tagged_data = osmnx.geometries_from_place("Germany", tags = tag)

# add snapshot year
tagged_data["snap_year"] = year

# reset index
tagged_data.reset_index(inplace = True)

# remove relations
tagged_data.drop(
    tagged_data[tagged_data.element_type == "relation"].index,
    inplace = True
)

# --------------------------------------------------
# export

tagged_data.to_csv(
    join(
        data_path,
        "townhall/townhalls_germany.csv"
    )
)