## Imports ----
library(tidyverse)

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
sav_folder <- cfig$data_dir

## Import data ----
prem_df <-
    readRDS(sprintf("%s/death_pop_stdpop_age12.RDS", sav_folder))

## Calculate age-specific rates for entire time period ----
## Subset to just premature mortality (< 65)
## Collapse sex and year, calculate age-specific rates.
age_specific_rates <- prem_df %>%
    dplyr::filter(age12 <= 9) %>%
    dplyr::mutate(black  = ifelse(racesex %in% 3:4, 1, 0)) %>%
    dplyr::group_by(fipschar, black, age12, age12_cat, pop_std) %>%
    dplyr::summarize(
        n_deaths = sum(n_deaths),
        pop = sum(pop),
        death_rate = n_deaths / pop * 10 ^ 5
    )

## Calculate age-standardized rates ----
age_standardized_rates <- age_specific_rates %>%
    dplyr::mutate(death_rate = ifelse(is.nan(death_rate), 0, death_rate)) %>%
    dplyr::group_by(fipschar, black) %>%
    dplyr::summarize(
        total_deaths = sum(n_deaths),
        total_pop = sum(pop),
        death_rate = stats::weighted.mean(death_rate, pop_std)
    )

## Save ----
saveRDS(age_standardized_rates,
        sprintf("%s/age_std_rates_by_race.RDS",
                sav_folder))
