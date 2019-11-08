library(tidyverse)
source('./code/helpers/99_helper_functions.R')

load('./data_working/model1_extracted_samples.RData')
load('./data_working/model2_extracted_samples.RData')

m1_phis_w <-
    summarize_by_columns(m1_samps$phi * m1_samps$delta) %>%
    dplyr::mutate(type  = "White",
                  model = "Model 1")
m1_phis_b <-
    summarize_by_columns(m1_samps$phi / m1_samps$delta) %>%
    dplyr::mutate(type  = "Black",
                  model = "Model 1")
m1_phis   <- summarize_by_columns(m1_samps$phi) %>%
    dplyr::mutate(type  = "Unscaled",
                  model = "Model 1")

m2_phis_w <-
    summarize_by_columns(m2_samps$phi * m2_samps$delta) %>%
    dplyr::mutate(type  = "White",
                  model = "Model 2")
m2_phis_b <-
    summarize_by_columns(m2_samps$phi / m2_samps$delta) %>%
    dplyr::mutate(type  = "Black",
                  model = "Model 2")
m2_phis   <- summarize_by_columns(m2_samps$phi) %>%
    dplyr::mutate(type  = "Unscaled",
                  model = "Model 2")

phis <- rbind(m1_phis_w, m1_phis_b, m1_phis,
              m2_phis_w, m2_phis_b, m2_phis)

saveRDS(phis, './data_working/scaled_and_unscaled_phis.RDS')
