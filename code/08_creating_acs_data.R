## Imports ----
library(tidyverse)
library(acs)
source('./code/helpers/misc_helpers.R')
source('./code/helpers/acs_helpers.R')

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
sav_folder  <- cfig$data_dir
acs_api_key <- cfig$acs_api_key
install_key <- cfig$install_key

## Make folders ----
mkdir_p(sav_folder)

## Install API key? ----
## Should only need to do this once per machine
if (install_key) {
    acs::api.key.install(key = acs_api_key)
}

## Make ACS geography -----
us_counties <- geo.make(state = "*", county = "*")

## Get the info and the data ----
b19013_info <- acs.lookup(2015, table.number = "B19013")
median_hh_income <- acs_to_df("B19013", geo = us_counties)
get_table_name(b19013_info)
print_table_vars(b19013_info)

## Save it ----
saveRDS(median_hh_income,
        sprintf("%s/median_hh_income.RDS", sav_folder))
