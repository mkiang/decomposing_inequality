## Imports ----
library(tidyverse)
source('./code/helpers/misc_helpers.R')

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
raw_folder <- cfig$raw_dir
sav_folder <- cfig$data_dir
year_0 <- cfig$start_year
year_n <- cfig$end_year

## Make folders ----
mkdir_p(sav_folder)

## Import data ----
## File is in fixed width format, so let's define the size of of the columns
## along with the names. See documentation for more.
u02_widths <- readr::fwf_widths(
    c(2, 3, 4, 1, 1, 2, 4, 3, 4),
    col_names = c(
        "fips_st",
        "fips_cnty",
        "year",
        "racesex",
        "hispanic",
        "agegroup",
        "ucod",
        "recode",
        "n_deaths"
    )
)
pop_widths <- readr::fwf_widths(
    c(2, 3, 4, 1, 1, rep(8, 14), 25, 1),
    col_names = c(
        "fips_st",
        "fips_cnty",
        "year",
        "racesex",
        "hispanic",
        "livebirths",
        "age1",
        "age2",
        "age3",
        "age4",
        "age5",
        "age6",
        "age7",
        "age8",
        "age9",
        "age10",
        "age11",
        "age12",
        "age13",
        "countyname",
        "record_type"
    )
)

## Actually do the importing -- subsetting to years we want right away
raw_mort <-
    readr::read_fwf(
        sprintf("%s/comp_mort_data/U02_1999_2015/MORT9915.txt",
                raw_folder),
        col_types = "ccnnnncnn",
        col_positions = u02_widths
    ) %>%
    dplyr::filter(year %in% year_0:year_n)
raw_pop  <-
    readr::read_fwf(
        sprintf("%s/comp_mort_data/U02_1999_2015/pop9915.txt",
                raw_folder),
        col_types = "ccnnnnnnnnnnnnnnnnncn",
        col_positions = pop_widths
    ) %>%
    dplyr::filter(year %in% year_0:year_n)

## Collapse down CMF data ----
## Sum up over underlying cause of death
cmf_df <- raw_mort %>%
    dplyr::group_by(fips_st, fips_cnty, year, racesex, hispanic, agegroup) %>%
    dplyr::summarize(n_deaths = sum(n_deaths)) %>%
    dplyr::ungroup()

## Add in categoricals to CMF ----
cmf_df <- cmf_df %>%
    dplyr::mutate(
        racesex_cat =
            factor(
                racesex,
                levels = 1:8,
                labels = c(
                    "White male",
                    "White female",
                    "Black male",
                    "Black female",
                    "AIAN male",
                    "AIAN female",
                    "API male",
                    "API female"
                ),
                ordered = TRUE
            ),
        hispanic_cat =
            factor(
                hispanic,
                levels = c(1, 2, 9),
                labels = c("not Hispanic or Latino",
                           "Hispanic or Latino",
                           "Unknown"),
                ordered = TRUE
            ),
        age_cat = factor(
            agegroup,
            levels = c(1:16, 99),
            labels = c(
                "<1 day",
                "1-6 days",
                "7-27 days",
                "28-364 days",
                "1-4 years",
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
                "85+ years",
                "Unknown"
            )
        )
    )

## Make a few new columns for convenience ----
## Also note that we make a new age variable called age13 that matches the
## 13 age groups inside of the population file. We also need to make an age12
## that matches the 12 groups of the standard population
cmf_df <- cmf_df %>%
    dplyr::mutate(
        fipschar = paste0(fips_st, fips_cnty),
        female = ifelse(racesex %% 2 == 0, 1, 0),
        male   = 1 - female,
        white  = ifelse(racesex %in% 1:2, 1, 0),
        black  = ifelse(racesex %in% 3:4, 1, 0),
        aian   = ifelse(racesex %in% 5:6, 1, 0),
        api    = ifelse(racesex %in% 7:8, 1, 0),
        age13  = ifelse(agegroup <= 4, 1, agegroup - 3),
        agecat = factor(
            age13,
            levels = 1:13,
            labels = c(
                "< 1 year",
                "1-4 years",
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
        ),
        age12  = ifelse(age13 <= 2, 1, age13 - 1),
        nhw    = ifelse(hispanic == 1 & white == 1, 1, 0),
        nhb    = ifelse(hispanic == 1 & black == 1, 1, 0)
    )

## Add in categoricals to pop ----
pop_data <- raw_pop  %>%
    dplyr::mutate(
        racesex_cat =
            factor(
                racesex,
                levels = 1:8,
                labels = c(
                    "White male",
                    "White female",
                    "Black male",
                    "Black female",
                    "AIAN male",
                    "AIAN female",
                    "API male",
                    "API female"
                ),
                ordered = TRUE
            ),
        hispanic_cat =
            factor(
                hispanic,
                levels = c(1, 2, 9),
                labels = c("not Hispanic or Latino",
                           "Hispanic or Latino",
                           "Unknown"),
                ordered = TRUE
            )
    )

## Slight munging for pop data ----
pop_data <- pop_data %>%
    dplyr::filter(record_type == 3) %>%
    dplyr::select(-livebirths, -record_type) %>%
    tidyr::gather(key = age13, value = pop, age1:age13) %>%
    dplyr::mutate(
        fipschar = paste0(fips_st, fips_cnty),
        countyname = stringr::str_to_title(countyname),
        countyname = factor(countyname),
        female = ifelse(racesex %% 2 == 0, 1, 0),
        male   = 1 - female,
        white  = ifelse(racesex %in% 1:2, 1, 0),
        black  = ifelse(racesex %in% 3:4, 1, 0),
        aian   = ifelse(racesex %in% 5:6, 1, 0),
        api    = ifelse(racesex %in% 7:8, 1, 0),
        age13  = as.numeric(substr(age13, 4, nchar(age13))),
        agecat = factor(
            age13,
            levels = 1:13,
            labels = c(
                "< 1 year",
                "1-4 years",
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
        ),
        age12  = ifelse(age13 <= 2, 1, age13 - 1),
        nhw    = ifelse(hispanic == 1 & white == 1, 1, 0),
        nhb    = ifelse(hispanic == 1 & black == 1, 1, 0)
    )

## Save ----
saveRDS(cmf_df,
        sprintf("%s/extracted_cmf_%s_to_%s.RDS",
                sav_folder, year_0, year_n))
saveRDS(pop_data,
        sprintf("%s/extracted_pop_%s_to_%s.RDS",
                sav_folder, year_0, year_n))
