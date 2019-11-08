library(tidyverse)
library(RColorBrewer)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(ggmap)
library(cowplot)

source('./code/helpers/99_helper_functions.R')
load('./data_working/lower_us_maps.RData')
fips_idx_map <- readRDS('./data_working/fips_to_dummy_mappings.RDS')

m1_sum <- readRDS('./stanfit_summaries/model0_no_covars_summ.RDS')
m2_sum <- readRDS('./stanfit_summaries/model1_with_income_summ.RDS')

re_1 <- reshape_reffects(m1_sum) %>%
    dplyr::left_join(fips_idx_map, by = "c_idx") %>%
    tibble::add_column(model = "Model 1") %>%
    dplyr::select(
        model,
        s_idx,
        c_idx,
        st_abbr,
        fips_st,
        fipschar,
        param,
        param_exp,
        mean,
        p500,
        p025,
        p975,
        n_eff,
        rstan::Rhat
    )
re_2 <- reshape_reffects(m2_sum) %>%
    dplyr::left_join(fips_idx_map, by = "c_idx") %>%
    tibble::add_column(model = "Model 2") %>%
    dplyr::select(
        model,
        s_idx,
        c_idx,
        st_abbr,
        fips_st,
        fipschar,
        param,
        param_exp,
        mean,
        p500,
        p025,
        p975,
        n_eff,
        rstan::Rhat
    )
nu_1 <- reshape_seffects(m1_sum) %>%
    dplyr::left_join(fips_idx_map %>%
                         dplyr::select(s_idx, fips_st, st_abbr) %>%
                         dplyr::distinct(),
                     by = "s_idx") %>%
    tibble::add_column(model = "Model 1") %>%
    dplyr::select(
        model,
        s_idx,
        st_abbr,
        fips_st,
        param,
        param_exp,
        mean,
        p500,
        p025,
        p975,
        n_eff,
        rstan::Rhat
    )
nu_2 <- reshape_seffects(m2_sum) %>%
    dplyr::left_join(fips_idx_map %>%
                         dplyr::select(s_idx, fips_st, st_abbr) %>%
                         dplyr::distinct(),
                     by = "s_idx") %>%
    tibble::add_column(model = "Model 2") %>%
    dplyr::select(
        model,
        s_idx,
        st_abbr,
        fips_st,
        param,
        param_exp,
        mean,
        p500,
        p025,
        p975,
        n_eff,
        rstan::Rhat
    )

county_effects <- rbind(re_1, re_2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(st_abbr = factor(st_abbr, levels = sort(unique(st_abbr)),
                                   ordered = TRUE)) %>%
    dplyr::group_by(param_exp, model) %>%
    dplyr::arrange(p500, .by_group = TRUE) %>%
    dplyr::mutate(x_rank = 1:dplyr::n())

state_effects <- rbind(nu_1, nu_2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(st_abbr = factor(st_abbr, levels = sort(unique(st_abbr)),
                                   ordered = TRUE))


## Psi 1
psi1plot_all_m1 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi1",
                      model == "Model 1"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_linerange(size = .02, alpha = .25) +
    ggplot2::geom_point(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = seq(-1, 1, .5),
                        color = "grey85",
                        alpha = .5) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Model 1")

psi1plot_all_m2 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi1",
                      model == "Model 2"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_linerange(size = .02, alpha = .25) +
    ggplot2::geom_point(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = seq(-1, 1, .5),
                        color = "grey85",
                        alpha = .5) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Model 2")

psi1plot_states_m1 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi1",
                      model == "Model 1"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_hline(
        data = state_effects %>%
            dplyr::filter(model == "Model 1"),
        ggplot2::aes(yintercept = p500),
        color = "red",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey75",
                        alpha = .5) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::scale_x_continuous(expand = c(0, 3)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::facet_wrap(~ st_abbr) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL)

psi1plot_states_m2 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi1",
                      model == "Model 2"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_hline(
        data = state_effects %>%
            dplyr::filter(model == "Model 2"),
        ggplot2::aes(yintercept = p500),
        color = "red",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey75",
                        alpha = .5) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::scale_x_continuous(expand = c(0, 3)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::facet_wrap(~ st_abbr) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL)

psi1_plots <- cowplot::plot_grid(
    psi1plot_all_m1,
    psi1plot_all_m2,
    psi1plot_states_m1,
    psi1plot_states_m2,
    align = "h",
    axis = "lr",
    rel_heights = c(3, 8),
    ncol = 2
)

ggplot2::ggsave(
    "./plots/psi1_caterpillar_plot.pdf",
    psi1_plots,
    height = 11,
    width = 8,
    scale = 1.5
)

## Psi 2
psi2plot_all_m1 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi2",
                      model == "Model 1"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_linerange(size = .02, alpha = .25) +
    ggplot2::geom_point(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = seq(-1, 1, .5),
                        color = "grey85",
                        alpha = .5) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Model 1")

psi2plot_all_m2 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi2",
                      model == "Model 2"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_linerange(size = .02, alpha = .25) +
    ggplot2::geom_point(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = seq(-1, 1, .5),
                        color = "grey85",
                        alpha = .5) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Model 2")

psi2plot_states_m1 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi2",
                      model == "Model 1"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_hline(
        data = state_effects %>%
            dplyr::filter(model == "Model 1"),
        ggplot2::aes(yintercept = p500),
        color = "red",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey75",
                        alpha = .5) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::scale_x_continuous(expand = c(0, 3)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::facet_wrap(~ st_abbr) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL)

psi2plot_states_m2 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "psi2",
                      model == "Model 2"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_hline(
        data = state_effects %>%
            dplyr::filter(model == "Model 2"),
        ggplot2::aes(yintercept = p500),
        color = "red",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey75",
                        alpha = .5) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::scale_x_continuous(expand = c(0, 3)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::facet_wrap(~ st_abbr) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL)

psi2_plots <- cowplot::plot_grid(
    psi2plot_all_m1,
    psi2plot_all_m2,
    psi2plot_states_m1,
    psi2plot_states_m2,
    align = "h",
    axis = "lr",
    rel_heights = c(3, 8),
    ncol = 2
)

ggplot2::ggsave(
    "./plots/psi2_caterpillar_plot.pdf",
    psi2_plots,
    height = 11,
    width = 8,
    scale = 1.5
)



phiplot_all_m1 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "phi",
                      model == "Model 1"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_linerange(size = .02, alpha = .25) +
    ggplot2::geom_point(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = seq(-1, 1, .5),
                        color = "grey85",
                        alpha = .5) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Model 1")

phiplot_all_m2 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "phi",
                      model == "Model 2"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_linerange(size = .02, alpha = .25) +
    ggplot2::geom_point(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = seq(-1, 1, .5),
                        color = "grey85",
                        alpha = .5) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Model 2")

phiplot_states_m1 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "phi",
                      model == "Model 1"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_hline(
        data = state_effects %>%
            dplyr::filter(model == "Model 1"),
        ggplot2::aes(yintercept = p500),
        color = "red",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey75",
                        alpha = .5) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::scale_x_continuous(expand = c(0, 3)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::facet_wrap(~ st_abbr) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL)

phiplot_states_m2 <- ggplot2::ggplot(
    data = county_effects %>%
        dplyr::filter(param == "phi",
                      model == "Model 2"),
    ggplot2::aes(
        x = x_rank,
        y = p500,
        ymin = p025,
        ymax = p975
    )
) +
    ggplot2::geom_hline(
        data = state_effects %>%
            dplyr::filter(model == "Model 2"),
        ggplot2::aes(yintercept = p500),
        color = "red",
        alpha = .5
    ) +
    ggplot2::geom_pointrange(size = .1, alpha = .5) +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey75",
                        alpha = .5) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::scale_x_continuous(expand = c(0, 3)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "none"
    ) +
    ggplot2::facet_wrap(~ st_abbr) +
    ggplot2::labs(x = NULL, y = NULL, title = NULL)

phi_plots <- cowplot::plot_grid(
    phiplot_all_m1,
    phiplot_all_m2,
    phiplot_states_m1,
    phiplot_states_m2,
    align = "h",
    axis = "lr",
    rel_heights = c(3, 8),
    ncol = 2
)

ggplot2::ggsave(
    "./plots/phi_caterpillar_plot.pdf",
    phi_plots,
    height = 11,
    width = 8,
    scale = 1.5
)
