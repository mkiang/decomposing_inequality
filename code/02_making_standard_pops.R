## Imports ----
library(tidyverse)
source('./code/helpers/download_helpers.R')
source('./code/helpers/misc_helpers.R')

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
raw_folder <- cfig$raw_dir
sav_folder <- cfig$data_dir

## Make folders ----
mkdir_p(sav_folder)

## Import data ----
standard_pop_loc <- sprintf("%s/standard_pops_all.RDS", raw_folder)
if (!file.exists(standard_pop_loc)) {
    standard_pops <- download_standard_pops()
    
    ## Save full standard pops ----
    saveRDS(standard_pops, standard_pop_loc)
    
} else {
    standard_pops <- readRDS(standard_pop_loc)
}

## Subset to 2000 US Census population ----
sub_std <- standard_pops %>%
    dplyr::filter(standard == "s204") %>%
    dplyr::select(-standard,
                  -standard_cat,
                  age18 = age,
                  age18_cat = age_cat)

## Regroup into age13 ----
std_age12 <- sub_std %>%
    dplyr::mutate(
        age12 = dplyr::case_when(
            age18 == 0  ~ 1,
            age18 == 5  ~ 2,
            age18 == 10 ~ 3,
            age18 == 15 ~ 4,
            age18 == 20 ~ 5,
            age18 == 25 ~ 6,
            age18 == 30 ~ 6,
            age18 == 35 ~ 7,
            age18 == 40 ~ 7,
            age18 == 45 ~ 8,
            age18 == 50 ~ 8,
            age18 == 55 ~ 9,
            age18 == 60 ~ 9,
            age18 == 65 ~ 10,
            age18 == 70 ~ 10,
            age18 == 75 ~ 11,
            age18 == 80 ~ 11,
            age18 == 85 ~ 12
        ),
        age12_cat = factor(
            age12,
            levels = 1:12,
            labels = c(
                "0-4 years",
                "5-9 years",
                "10-14 years",
                "15-19 years",
                "20-24 years",
                "25-34 years",
                "35-44 years",
                "45-54 years",
                "55-64 years",
                "65-74 years",
                "74-84 years",
                "85+ years"
            )
        )
    ) %>%
    dplyr::group_by(age12, age12_cat) %>%
    dplyr::summarize(pop_std = sum(pop_std))

## Save ----
readr::write_csv(std_age12, sprintf("%s/standard_pop_us2000.csv", sav_folder))
