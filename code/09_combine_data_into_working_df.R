## Imports ----
library(tidyverse)

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
sav_folder <- cfig$data_dir
save_full_adj <- cfig$save_full_adj

## Import data ----
fips_map <-
    readRDS(sprintf("%s/fips_to_dummy_mappings.RDS", sav_folder))
prem_df <-
    readRDS(sprintf("%s/prem_observed_expected_counts.RDS", sav_folder))
income <- readRDS(sprintf("%s/median_hh_income.RDS", sav_folder))

## Merge data together ----
working_data <- prem_df %>%
    dplyr::left_join(fips_map, by = "fipschar") %>%
    dplyr::left_join(income, by = "fipschar") %>%
    dplyr::rename(med_hh_income = b19013_001) %>%
    dplyr::mutate(black = as.integer(black),
                  observed = as.integer(observed))

## Save ----
saveRDS(working_data,
        sprintf("%s/premature_working_data.RDS", sav_folder))
