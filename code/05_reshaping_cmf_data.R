## Imports ----
library(tidyverse)
source('./code/helpers/misc_helpers.R')

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
sav_folder <- cfig$data_dir
year_0     <- cfig$start_year
year_n     <- cfig$end_year

## Make folders ----
mkdir_p(sav_folder)

## Import data ----
cmf_df <- readRDS(sprintf("%s/extracted_cmf_%s_to_%s.RDS",
                          sav_folder, year_0, year_n))
pop_df <- readRDS(sprintf("%s/extracted_pop_%s_to_%s.RDS",
                          sav_folder, year_0, year_n))
std_pop <-
    readr::read_csv(sprintf("%s/standard_pop_us2000.csv", sav_folder))
fips_to_use <-
    readr::read_csv(sprintf("%s/county_adj_with_dummy.csv",
                            sav_folder)) %>%
    dplyr::pull(fips_i) %>%
    unique

## Subset ----
## Just get FIPS we want and subset to nonHispanic black and nonHispanic white
sub_cmf <- cmf_df %>%
    dplyr::filter(nhw + nhb == 1,
                  fipschar %in% fips_to_use) %>%
    dplyr::select(fipschar,
                  year,
                  racesex,
                  racesex_cat,
                  agegroup,
                  age13,
                  age12,
                  n_deaths)

sub_pop <- pop_df %>%
    dplyr::filter(nhw + nhb == 1,
                  fipschar %in% fips_to_use) %>%
    dplyr::select(fipschar, year, racesex, racesex_cat,
                  age13, age12, pop)

## Aggregate over age using the 12 groups of the standardized ----
cmf_age12 <- sub_cmf %>%
    dplyr::group_by(fipschar, year, racesex, age12) %>%
    dplyr::summarize(n_deaths = sum(n_deaths))

pop_age12 <- sub_pop %>%
    dplyr::group_by(fipschar, year, racesex, racesex_cat, age12) %>%
    dplyr::summarize(pop = sum(pop))

## Aggregate over age using the 13 groups of the population file ----
cmf_age13 <- sub_cmf %>%
    dplyr::group_by(fipschar, year, racesex, age13) %>%
    dplyr::summarize(n_deaths = sum(n_deaths))

pop_age13 <- sub_pop %>%
    dplyr::group_by(fipschar, year, racesex, racesex_cat, age13) %>%
    dplyr::summarize(pop = sum(pop))

## Join death and population data ----
prem_age12 <- pop_age12 %>%
    dplyr::left_join(cmf_age12, by = c("fipschar", "year", "racesex", "age12"))

prem_age13 <- pop_age13 %>%
    dplyr::left_join(cmf_age13, by = c("fipschar", "year", "racesex", "age13"))

## Replace NA deaths with 0 ----
prem_age12 <-
    tidyr::replace_na(prem_age12, replace = list(n_deaths = 0))

prem_age13 <-
    tidyr::replace_na(prem_age13, replace = list(n_deaths = 0))


## Add standard pop to 12 age group dataframe ----
prem_age12 <- prem_age12 %>%
    dplyr::left_join(std_pop, by = "age12")


## Save ----
saveRDS(prem_age12,
        sprintf("%s/death_pop_stdpop_age12.RDS", sav_folder))
saveRDS(prem_age13, sprintf("%s/death_pop_age13.RDS", sav_folder))
