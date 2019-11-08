## Imports
library(tidyverse)

raw_df <- readRDS("./data_working/death_pop_age13.RDS")
raw_df <- raw_df %>%
    dplyr::mutate(agecat = factor(
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
    ))

prem_df <- raw_df %>%
    dplyr::filter(age13 <= 10) %>%
    dplyr::mutate(female = ifelse(racesex %% 2 == 0, 1, 0),
                  black  = ifelse(racesex %in% 3:4, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, female, black, age = age13, agecat, n_deaths, pop)

prem_df <- prem_df %>%
    dplyr::mutate(
        age_orig = age,
        age = dplyr::case_when(
            age_orig <= 2 ~ 0,
            age_orig > 2 & age_orig < 8 ~ (age_orig - 2) * 5,
            age_orig == 8 ~ 35,
            age_orig == 9 ~ 45,
            age_orig == 10 ~ 55
        )
    )

prem_df <- prem_df %>%
    dplyr::group_by(year, age) %>%
    dplyr::summarize(deaths = sum(n_deaths),
                     pop = sum(pop))

std_pop <- narcan::std_pops %>%
    dplyr::filter(standard == "s204") %>%
    dplyr::select(age, age_cat, pop_std)

std_pop <- std_pop %>%
    dplyr::mutate(age = dplyr::case_when(age == 30 ~ 25,
                                         age == 40 ~ 35,
                                         age == 50 ~ 45,
                                         age == 60 ~ 55,
                                         TRUE ~ age)) %>%
    dplyr::group_by(age) %>%
    dplyr::summarize(pop_std = sum(pop_std))

prem_df <- prem_df %>%
    dplyr::left_join(std_pop, by = "age")

age_spec_rates <- prem_df %>%
    dplyr::mutate(prem_death_rate = deaths / pop * 100000)

us_age_std_rate <- age_spec_rates %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        age_std_rate = stats::weighted.mean(prem_death_rate, pop_std)
        )

print(us_age_std_rate)
print(mean(us_age_std_rate$age_std_rate))
