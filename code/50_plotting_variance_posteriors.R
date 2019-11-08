## Imports ----
library(magrittr)
library(rstan)
library(tidyverse)
library(matrixStats)
source('./code/99_helper_functions.R')


## Load data ----
load('./data_working/model1_extracted_samples.RData')
load('./data_working/model2_extracted_samples.RData')


## Make new dataframes for sigma_s ----
sig_s  <- c(m1_samps$sigma_s, m2_samps$sigma_s)
models <- rep(c("Model 1", "Model 2"),
              each = length(sig_s) / 2)
params <- "Sigma_s"
sigma_df <- data.frame(
    value = sig_s,
    model = models,
    m = models,
    param = params,
    p = params
)

sigmas_plot <- ggplot2::ggplot(sigma_df, ggplot2::aes(x = value,
                                                      color = model)) +
    ggplot2::geom_density(fill = NA) +
    ggplot2::labs(
        x = NULL,
        y = NULL,
        title = "Posterior distribution of sigma_s",
        subtitle = "Standard deviation of the state random effect"
    ) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::theme_classic() +
    ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = c(.99, .99),
        legend.justification = c(1, 1)
    ) +
    ggplot2::scale_x_continuous("Estimate", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(NULL, expand = c(0, 0))

ggplot2::ggsave(
    sigmas_plot,
    filename = './plots/sigma_s_posterior_distribution.pdf',
    width = 4,
    height = 4,
    scale = 1.25
)


## Variances for Psi1, psi2, phi
values  <- c(
    m1_samps$sigma_u1,
    m1_samps$sigma_v1,
    m1_samps$sigma_u2,
    m1_samps$sigma_v2,
    m1_samps$sigma_u3,
    m1_samps$sigma_v3,
    m2_samps$sigma_u1,
    m2_samps$sigma_v1,
    m2_samps$sigma_u2,
    m2_samps$sigma_v2,
    m2_samps$sigma_u3,
    m2_samps$sigma_v3
)
models <- rep(c("Model 1", "Model 2"), each = length(values) / 2)
params <- rep(rep(c("Psi1", "Psi2", "Phi"), 2),
              each = length(values) / 6)
u_or_v <- rep(rep(c("Spatial", "Unstr"), 6),
              each = length(values) / 12)
uv_df <- data.frame(
    value = values,
    model = models,
    m = models,
    param = params,
    p = params,
    type = u_or_v
)

uv_plots <-
    ggplot2::ggplot(uv_df, ggplot2::aes(x = value, color = type)) +
    ggplot2::geom_density(fill = NA) +
    ggplot2::facet_grid(model ~ param) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::theme_classic() +
    ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = c(.99, .99),
        legend.justification = c(1, 1)
    )  +
    ggplot2::scale_x_continuous("Estimate", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(NULL, expand = c(0, 0)) +
    ggplot2::labs(
        x = NULL,
        y = NULL,
        title = "Posterior distribution of BYM variance parameters",
        subtitle = "Top row are variance parameters for Model 1. Bottom for Model 2.\nLeft column are for the shared surface. Middle for the white surface. Right for the black surface.\nBlue is for the independent normally distributed portion. Red is for the conditional autoregressive portion."
    )
ggplot2::ggsave(
    uv_plots,
    filename = './plots/sigma_uv_posterior_distribution.pdf',
    width = 8,
    height = 4,
    scale = 1.25
)
