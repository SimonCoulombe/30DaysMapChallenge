#https://open.canada.ca/data/en/dataset/522b07b9-78e2-4819-b736-ad9208eb1067
library(tidyverse)
library(sf)
library(mapview)
z <- read_sf("2021_17_land/data/AL_TA_QC_2_137_fra.shp")
z2 <- read_sf("2021_17_land/data/AL_TA_QC_2_137_CONFIRME_fra.shp")
mapview(z, zcol ="TYPETA")

mapview(z, zcol ="TYPETA")

