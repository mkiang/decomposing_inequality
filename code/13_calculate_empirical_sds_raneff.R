library(tidyverse)
library(rstan)
library(matrixStats)

source('./code/helpers/99_helper_functions.R')

m1_fit <- readRDS('./stanfit_objects/model0_no_covars.RDS')
m2_fit <- readRDS('./stanfit_objects/model1_with_income.RDS')

## Model 1
m1_params <- as.matrix(m1_fit, pars = c("phi", "psi"))
m1_psis <-
    matrixStats::rowSds(m1_params[, grepl(x = colnames(m1_params),
                                          pattern = "phi[",
                                          fixed = TRUE)])
m1_phi1s <-
    matrixStats::rowSds(m1_params[, grepl(x = colnames(m1_params),
                                          pattern = "psi[1,",
                                          fixed = TRUE)])
m1_phi2s <-
    matrixStats::rowSds(m1_params[, grepl(x = colnames(m1_params),
                                          pattern = "psi[2,",
                                          fixed = TRUE)])
rm(m1_params, m1_fit)

## Model 2
m2_params <- as.matrix(m2_fit, pars = c("phi", "psi"))
m2_psis <-
    matrixStats::rowSds(m2_params[, grepl(x = colnames(m2_params),
                                          pattern = "phi[",
                                          fixed = TRUE)])
m2_phi1s <-
    matrixStats::rowSds(m2_params[, grepl(x = colnames(m2_params),
                                          pattern = "psi[1,",
                                          fixed = TRUE)])
m2_phi2s <-
    matrixStats::rowSds(m2_params[, grepl(x = colnames(m2_params),
                                          pattern = "psi[2,",
                                          fixed = TRUE)])
rm(m2_params, m2_fit)

## Reshaping
values <- c(m1_psis, m1_phi1s, m1_phi2s,
            m2_psis, m2_phi1s, m2_phi2s)
models <- rep(c("Model 1", "Model 2"), each = length(values) / 2)
params <-
    rep(rep(c("Psi", "Phi1", "Phi2"), 2), each = length(values) / 6)

empirical_sds <- dplyr::as_data_frame(list(
    model = models,
    param = params,
    value = values
))

## Save
saveRDS(empirical_sds, file = './data_working/empirical_sds.RDS')
