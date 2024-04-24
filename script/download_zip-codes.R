library(data.table)
library(sf)
library(ggplot2)

### from https://suche-postleitzahl.org ------------
# download zip codes and additional info
dir.create("data/geodata/zip-codes", showWarnings = FALSE)
download_links = c(
  "https://download-v2.suche-postleitzahl.org/wgs84/exakt/plz-5stellig/geojson/plz-5stellig.geojson",
  "https://download-v2.suche-postleitzahl.org/wgs84/exakt/plz-5stellig/geojson/points/plz-5stellig-centroid.geojson",
  "https://download-v2.suche-postleitzahl.org/data/plz-5stellig-daten.csv"
)
options(timeout = 5 * 60)
for (url in download_links) {
  fpath = paste0("data/geodata/zip-codes/", basename(url))
  if (!file.exists(fpath))
    download.file(url, fpath)
}


## the zipcode of the city hall ----
## info: https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122019_Auszug_GV.html;jsessionid=4363531E637EF8C6210F835014F36679.live742
url = "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122019_Auszug_GV.xlsx?__blob=publicationFile"

fpath = "data/31122019_Auszug_GV.xlsx"
if (!file.exists(fpath)) download.file(url, fpath)
