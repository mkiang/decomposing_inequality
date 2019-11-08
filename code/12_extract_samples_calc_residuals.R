## Imports ----
library(rstan)
library(tidyverse)
library(matrixStats)
source('./code/helpers/99_helper_functions.R')

## Define the level of significance ----
sig_limit <- .8

## Mapping data ----
fips_idx_map <- readRDS('./data_working/fips_to_dummy_mappings.RDS')

## Get model fits ----
## Note: I named model 1 model 0 because I was using index as number of
## covariates. Rename here for the paper (where Model 1 will be null model)
m1_path  <- './stanfit_objects/model0_no_covars.RDS'
m1_fit   <- readRDS(m1_path)
m1_samps <- extract_samples(m1_fit)
rm(m1_fit, m1_path)
gc()

m2_path  <- './stanfit_objects/model1_with_income.RDS'
m2_fit   <- readRDS(m2_path)
m2_samps <- extract_samples(m2_fit)
rm(m2_fit, m2_path)
gc()

## Residual disparitiy ----
m1_res_disp <- calc_residual_disparity(m1_samps)
m2_res_disp <- calc_residual_disparity(m2_samps)

## Total disparity ----
m1_tot_disp <- (m1_samps$alpha2 - m1_samps$alpha1) + m1_res_disp
m2_tot_disp <- (m2_samps$alpha2 - m2_samps$alpha1) + m2_res_disp

## Figure out the components of total disparity ----
diff_in_spec <- rbind(
    summarize_by_columns(m1_samps$psi2 - m1_samps$psi1) %>%
        dplyr::select(c_idx, diff_in_spec = p500) %>%
        dplyr::mutate(model_name = "Model 1"),
    summarize_by_columns(m2_samps$psi2 - m2_samps$psi1) %>%
        dplyr::select(c_idx, diff_in_spec = p500) %>%
        dplyr::mutate(model_name = "Model 2")
)

phis <- rbind(
    summarize_by_columns(m1_samps$phi) %>%
        dplyr::select(c_idx, phi = p500) %>%
        dplyr::mutate(model_name = "Model 1"),
    summarize_by_columns(m2_samps$phi) %>%
        dplyr::select(c_idx, phi = p500) %>%
        dplyr::mutate(model_name = "Model 2")
)

diff_in_alphas <- rbind(
    dplyr::as_tibble(list(
        diff_in_alpha =
            stats::median(m1_samps$alpha2 - m1_samps$alpha1)
    )) %>%
        dplyr::mutate(model_name = "Model 1"),
    dplyr::as_tibble(list(
        diff_in_alpha =
            stats::median(m2_samps$alpha2 - m2_samps$alpha1)
    )) %>%
        dplyr::mutate(model_name = "Model 2")
)

recip_delta_minus_delta <- rbind(
    dplyr::as_tibble(list(
        recip_delta_minus_delta =
            stats::median(1 / m1_samps$delta - m1_samps$delta)
    )) %>%
        dplyr::mutate(model_name = "Model 1"),
    dplyr::as_tibble(list(
        recip_delta_minus_delta =
            stats::median(1 / m2_samps$delta - m2_samps$delta)
    )) %>%
        dplyr::mutate(model_name = "Model 2")
)

parts <- diff_in_spec %>%
    dplyr::left_join(phis) %>%
    dplyr::left_join(diff_in_alphas) %>%
    dplyr::left_join(recip_delta_minus_delta) %>%
    dplyr::mutate(
        total_disp = diff_in_alpha + phi * recip_delta_minus_delta +
            diff_in_spec,
        frac_alphas = diff_in_alpha / total_disp,
        frac_shared = (phi * recip_delta_minus_delta) / total_disp,
        frac_spec   = diff_in_spec / total_disp,
        sum_fracs   = frac_alphas + frac_shared + frac_spec
    )

## Save samples ----
save(m1_samps, file = './data_working/model1_extracted_samples.RData')
save(m2_samps, file = './data_working/model2_extracted_samples.RData')
saveRDS(parts, file = "./data_working/proportion_of_inequality.RDS")
rm(m1_samps, m2_samps, parts)

## Summarize and combine them ----
m1m2_tdisp <- rbind(
    summarize_by_columns(m1_tot_disp) %>%
        tibble::add_column(model_name = "Model 1"),
    summarize_by_columns(m2_tot_disp) %>%
        tibble::add_column(model_name = "Model 2")
) %>%
    tibble::add_column(disp_type = "total")
m1m2_rdisp <- rbind(
    summarize_by_columns(m1_res_disp) %>%
        tibble::add_column(model_name = "Model 1"),
    summarize_by_columns(m2_res_disp) %>%
        tibble::add_column(model_name = "Model 2")
) %>%
    tibble::add_column(disp_type = "residual")

resid_df <- rbind(m1m2_tdisp, m1m2_rdisp)

rm(m1m2_rdisp,
   m1m2_tdisp,
   m1_res_disp,
   m2_res_disp,
   m1_tot_disp,
   m2_tot_disp)
gc()

## Define "significant" counties as > X% above or below 0. ----
resid_df <- resid_df %>%
    dplyr::mutate(sig = dplyr::case_when(n_neg / count >= sig_limit ~ 1,
                                         n_pos / count >= sig_limit ~ 1,
                                         TRUE ~ 0))

## Add in correct fipschar ----
resid_df <- resid_df %>%
    dplyr::left_join(fips_idx_map, by = "c_idx")

## Save it ----
save(resid_df, file = './data_working/residual_disparities_data.RData')
