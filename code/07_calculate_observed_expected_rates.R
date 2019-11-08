## Imports ----
library(tidyverse)
source('./code/helpers/misc_helpers.R')

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
sav_folder <- cfig$data_dir

## Import data ----
raw_df <- readRDS(sprintf("%s/death_pop_age13.RDS", sav_folder))

## Take out >65 year olds and make female variable ----
prem_df <- raw_df %>%
    dplyr::filter(age13 <= 10) %>%
    dplyr::mutate(female = ifelse(racesex %% 2 == 0, 1, 0),
                  black  = ifelse(racesex %in% 3:4, 1, 0)) %>%
    dplyr::ungroup()

## Get the standard rates ----
## We just get the standard rate for each sex/age group combination
## combining all observations over the time period (over fips).
std_pop <- prem_df %>%
    dplyr::group_by(female, age13) %>%
    dplyr::summarize(
        observed = sum(n_deaths),
        person_years = sum(pop),
        std_rate = observed / person_years
    )

## Aggregate over years ----
## Just tally up the number of person years and deaths over all years
prem_df <- prem_df %>%
    dplyr::group_by(fipschar, female, black, age13) %>%
    dplyr::summarize(person_years = sum(pop),
                     observed = sum(n_deaths))

## Merge with std_pop ----
prem_df <- prem_df %>%
    dplyr::left_join(std_pop %>%
                         dplyr::select(female, age13, std_rate),
                     by = c("female", "age13"))

## Expected age/sex counts ----
## Now we multiply the standard (national) age/sex rate to get expected rates
prem_df <- prem_df %>%
    dplyr::mutate(expected = std_rate * person_years)

## Now aggregate age ----
prem_df <- prem_df %>%
    dplyr::group_by(fipschar, black) %>%
    dplyr::summarize(observed = sum(observed),
                     expected = sum(expected)) %>%
    dplyr::arrange(black, fipschar) %>%
    dplyr::ungroup()

## Save ----
saveRDS(prem_df,
        sprintf("%s/prem_observed_expected_counts.RDS", sav_folder))
