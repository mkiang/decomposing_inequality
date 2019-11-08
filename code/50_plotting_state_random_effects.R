## Imports ----
library(magrittr)
library(rstan)
library(tidyverse)
library(matrixStats)
source('./code/helpers/99_helper_functions.R')


## Load data ----
load('./data_working/model1_extracted_samples.RData')
load('./data_working/model2_extracted_samples.RData')
fips_idx_map <- readRDS('./data_working/fips_to_dummy_mappings.RDS')

## Make a new summary dataframe ----
m1_nu <- summarize_by_columns(m1_samps$nu) %>%
    dplyr::rename(s_idx = c_idx) %>%
    dplyr::mutate(model = "Model 1")

m2_nu <- summarize_by_columns(m2_samps$nu) %>%
    dplyr::rename(s_idx = c_idx) %>%
    dplyr::mutate(model = "Model 2")

nu_df <- rbind(m1_nu, m2_nu) %>%
    dplyr::left_join(fips_idx_map %>%
                         dplyr::select(fips_st, s_idx, st_abbr) %>%
                         dplyr::distinct(),
                     by = "s_idx") %>%
    dplyr::mutate(model = factor(
        model,
        levels = c("Model 2", "Model 1"),
        ordered = TRUE
    ))

nu_plot <- ggplot2::ggplot(nu_df,
                           ggplot2::aes(
                               x = st_abbr,
                               y = avg,
                               ymax = p975,
                               ymin = p025,
                               color = model
                           )) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(position = ggplot2::position_dodge(.5)) +
    ggplot2::geom_linerange(position = ggplot2::position_dodge(.5)) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = c(0, 1),
                   legend.justification = c(0, 1)) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::labs(title = "Posterior distribution of\nstate random effects (nu)",
                  x = NULL, y = NULL)

ggplot2::ggsave(
    nu_plot,
    filename = './plots/nu_posterior_distribution.pdf',
    height = 7,
    width = 3,
    scale = 1.25
)
