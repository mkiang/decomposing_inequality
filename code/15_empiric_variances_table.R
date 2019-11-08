## Imports
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(ggmap)
library(matrixStats)

## Load helpers and helper data
source('./code/helpers/99_helper_functions.R')
load('./data_working/lower_us_maps.RData')
fips_idx_map <-
    readRDS('./data_working/fips_to_dummy_mappings.RDS')
prem <- readRDS("./data_working/premature_working_data.RDS") %>%
    dplyr::mutate(
        p500 = observed / expected,
        param = dplyr::case_when(black == 0 ~ "white_smr",
                                 black == 1 ~ "black_smr"),
        model_name = "raw"
    )

quick_summarize <- function(x) {
    new_df <- x %>%
        dplyr::as_data_frame() %>%
        dplyr::summarize(
            p500 = stats::median(value),
            mean = mean(value),
            p025 = stats::quantile(value, .025),
            p975 = stats::quantile(value, .975)
        )
    return(new_df)
}

## Load random effects summary data
m1_sum <- readRDS('./stanfit_summaries/model0_no_covars_summ.RDS')
m2_sum <- readRDS('./stanfit_summaries/model1_with_income_summ.RDS')

## Load extract samples
load("./data_working/model1_extracted_samples.RData")
load("./data_working/model2_extracted_samples.RData")

## Make a version of nu that is expanded such that every state column
## repeats as many times as their are counties. Then we can just add
## the expanded matrix when calculating posterior quantities.
nu_counts <- fips_idx_map %>%
    dplyr::group_by(s_idx) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(c_sum = cumsum(count))

## Initialize a holder
nu_expanded_1 <-
    nu_expanded_2 <- matrix(NA,
                            nrow = nrow(m1_samps$psi1),
                            ncol = max(nu_counts$c_sum))

for (s in 1:nrow(nu_counts)) {
    start_ix <- ifelse(length(nu_counts$c_sum[s - 1]) > 0,
                       nu_counts$c_sum[s - 1], 0) + 1
    end_ix <- nu_counts$c_sum[s]
    
    nu_expanded_1[, start_ix:end_ix] <- m1_samps$nu[, s]
    nu_expanded_2[, start_ix:end_ix] <- m2_samps$nu[, s]
}

empiric_var_risks <- rbind(
    matrixStats::rowVars(
        m1_samps$alpha1 + m1_samps$psi1 +
            (m1_samps$phi * m1_samps$delta) +
            nu_expanded_1
    )  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "white"),
    matrixStats::rowVars(
        m2_samps$alpha1 + m2_samps$psi1 +
            (m2_samps$delta * m2_samps$phi) +
            nu_expanded_1
    ) %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "white"),
    matrixStats::rowVars(
        m1_samps$alpha2 + m1_samps$psi2 +
            (m1_samps$phi / m1_samps$delta) +
            nu_expanded_2
    ) %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "black"),
    matrixStats::rowVars(
        m2_samps$alpha2 + m2_samps$psi2 +
            (m2_samps$phi / m2_samps$delta) +
            nu_expanded_2
    ) %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "black")
) %>%
    dplyr::mutate(param = "total risk")

empiric_shared_risk <- rbind(
    matrixStats::rowVars(m1_samps$phi * m1_samps$delta)  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "white"),
    matrixStats::rowVars(m2_samps$phi * m1_samps$delta) %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "white"),
    matrixStats::rowVars(m1_samps$phi / m1_samps$delta)  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "black"),
    matrixStats::rowVars(m2_samps$phi / m1_samps$delta) %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "black")
) %>%
    dplyr::mutate(param = "shared risk")

empiric_specific_risks <- rbind(
    matrixStats::rowVars(m1_samps$psi1)  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "white"),
    matrixStats::rowVars(m2_samps$psi1)  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "white"),
    matrixStats::rowVars(m1_samps$psi2)  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "black"),
    matrixStats::rowVars(m2_samps$psi2)  %>%
        quick_summarize() %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "black")
) %>%
    dplyr::mutate(param = "specific risk")

empiric_shared_var_frac <- rbind(
    quick_summarize(
        matrixStats::rowVars(m1_samps$phi * m1_samps$delta) /
            (
                matrixStats::rowVars(m1_samps$phi * m1_samps$delta) +
                    matrixStats::rowVars(m1_samps$psi1)
            )
    ) %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "white"),
    quick_summarize(
        matrixStats::rowVars(m2_samps$phi * m2_samps$delta) /
            (
                matrixStats::rowVars(m2_samps$phi * m2_samps$delta) +
                    matrixStats::rowVars(m2_samps$psi1)
            )
    ) %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "white"),
    quick_summarize(
        matrixStats::rowVars(m1_samps$phi / m1_samps$delta) /
            (
                matrixStats::rowVars(m1_samps$phi / m1_samps$delta) +
                    matrixStats::rowVars(m1_samps$psi2)
            )
    ) %>%
        dplyr::mutate(model_name = "Model 1",
                      race = "black"),
    quick_summarize(
        matrixStats::rowVars(m2_samps$phi / m2_samps$delta) /
            (
                matrixStats::rowVars(m2_samps$phi / m2_samps$delta) +
                    matrixStats::rowVars(m2_samps$psi2)
            )
    ) %>%
        dplyr::mutate(model_name = "Model 2",
                      race = "black")
) %>%
    dplyr::mutate(param = "fraction shared risk")

state_vars <- rbind(
    quick_summarize(m1_samps$sigma_s ^ 2) %>%
        dplyr::mutate(
            model_name = "Model 1",
            race = "both",
            param = "state variance"
        ),
    quick_summarize(m2_samps$sigma_s ^ 2) %>%
        dplyr::mutate(
            model_name = "Model 2",
            race = "both",
            param = "state variance"
        )
)

empiric_vars <- rbind(
    empiric_var_risks,
    state_vars,
    empiric_shared_risk,
    empiric_specific_risks,
    empiric_shared_var_frac
) %>%
    dplyr::select(model_name, race, param, dplyr::everything()) %>%
    dplyr::mutate(
        param = factor(
            param,
            levels =  c(
                "total risk",
                "shared risk",
                "specific risk",
                "fraction shared risk",
                "state variance"
            ),
            ordered = TRUE
        ),
        race = factor(
            race,
            levels = c("white", "black", "both"),
            ordered = TRUE
        )
    )
empiric_vars <- empiric_vars %>%
    dplyr::arrange(param, model_name, race)

saveRDS(empiric_vars, "./data_working/empiric_variance_table.RDS")
