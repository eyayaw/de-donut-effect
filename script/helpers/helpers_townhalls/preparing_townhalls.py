# --------------------------------------------------
# import libraries

import os
import pandas as pd
from os.path import join
import geopandas as gpd

# --------------------------------------------------
# paths

main_path = "H:/work/OSM-for-FTC/"
data_path = main_path + "data/"

# --------------------------------------------------
# read data

townhalls = pd.read_csv(
    join(
        data_path,
        "townhall/townhalls_germany.csv"
    )
)

# transform into spatial data
townhalls["geometry"] = gpd.GeoSeries.from_wkt(townhalls["geometry"])
townhalls_spatial = gpd.GeoDataFrame(townhalls, geometry = "geometry", crs = "EPSG:4326")


# if way -> use centroid as alternative
townhalls_spatial.loc[
    (townhalls_spatial.element_type == "way"), "geometry"
] = townhalls_spatial["geometry"].centroid

# --------------------------------------------------
# export

townhalls_spatial.to_file(
    join(
        data_path,
        "townhall/townhalls_germany_prep.shp"
    )
)