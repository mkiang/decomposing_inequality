## Download the 2015 CB shapefiles (20m will do) for counties and states:
## https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
## https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html

## Imports ----
library(tidyverse)
library(broom)
library(maptools)
library(rgdal)

## Load dummy code dictionary
load('./data_working/premature_0812.RData')

## Load the US state constants
source('./code/99_us_constants.R')

shp_res <- "20m"
shp_folder <- "./data_raw/shapefiles"

## (1) Download and extract 2015 US Census Cartographic Boundaries ----
utils::download.file(
    url = paste0(
        "http://www2.census.gov/geo/tiger/GENZ2013/",
        sprintf("cb_2013_us_county_%s.zip", shp_res)
    ),
    destfile = sprintf("%s/cb_2013_us_county_%s.zip",
                       shp_folder, shp_res)
)
utils::unzip(
    zipfile = sprintf("%s/cb_2013_us_county_%s.zip",
                      shp_folder, shp_res),
    exdir = shp_folder
)

utils::download.file(
    url = paste0(
        "http://www2.census.gov/geo/tiger/GENZ2013/",
        sprintf("cb_2013_us_state_%s.zip", shp_res)
    ),
    destfile = sprintf("%s/cb_2013_us_state_%s.zip",
                       shp_folder, shp_res)
)
utils::unzip(
    zipfile = sprintf("%s/cb_2013_us_state_%s.zip", shp_folder, shp_res),
    exdir = shp_folder
)

##  Import US states and counties from 2015 CB shapefile ----
##  We could use map_data() -- but want this to be generalizable to all shp.
allcounties <- rgdal::readOGR(dsn = "./data_raw/shapefiles/",
                              layer = "cb_2013_us_county_20m",
                              stringsAsFactors = FALSE)
allstates   <- rgdal::readOGR(dsn = "./data_raw/shapefiles/",
                              layer = "cb_2013_us_state_20m",
                              stringsAsFactors = FALSE)

## A little munging and subsetting for maps ----
allcounties@data$fipschar <- allcounties@data$GEOID
allcounties@data$state    <- allcounties@data$STATEFP
allstates@data$state      <- allstates@data$STATEFP

## Only use lower 48 states
subcounties <-
    subset(allcounties, allcounties@data$state %in% us$allb)
substates   <- subset(allstates, allstates@data$state %in% us$allb)

## Fortify into dataframes
subcounties_df <- broom::tidy(subcounties, region = "GEOID")
substates_df   <- broom::tidy(substates, region = "GEOID")
allcounties_df <- broom::tidy(allcounties, region = "GEOID")
allstates_df   <- broom::tidy(allstates, region = "GEOID")

save(subcounties_df, subcounties, substates, substates_df,
     file = './data_working/lower_us_maps.RData')

save(allcounties, allcounties_df, allstates, allstates_df,
     file = './data_working/all_us_maps.RData')
