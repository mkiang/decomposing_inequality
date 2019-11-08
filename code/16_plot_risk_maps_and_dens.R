## Imports
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(ggmap)

## Load helpers and helper data
source('./code/helpers/99_helper_functions.R')
load('./data_working/lower_us_maps.RData')
fips_idx_map <-
    readRDS('./data_working/fips_to_dummy_mappings.RDS')

## Load random effects summary data
m1_sum <- readRDS('./stanfit_summaries/model0_no_covars_summ.RDS')
m2_sum <- readRDS('./stanfit_summaries/model1_with_income_summ.RDS')

## Load extract samples
load("./data_working/model1_extracted_samples.RData")
load("./data_working/model2_extracted_samples.RData")

## Reshape random effects summary into usable form
re_1 <-
    reshape_reffects(m1_sum) %>%
    dplyr::left_join(fips_idx_map, by = "c_idx")
re_2 <-
    reshape_reffects(m2_sum) %>%
    dplyr::left_join(fips_idx_map, by = "c_idx")

reffs <- rbind(
    dplyr::mutate(re_1, model_name = "Model~1"),
    dplyr::mutate(re_2, model_name = "Model~2")
)

## Clean up
rm(m1_sum, m2_sum, re_1, re_2)
gc()

## Now we want the posterior probability of each county random effect
post_prob_re <- rbind(
    summarize_by_columns(m1_samps$psi1) %>%
        dplyr::mutate(param = "psi1",
                      model_name = "Model~1"),
    summarize_by_columns(m1_samps$psi2) %>%
        dplyr::mutate(param = "psi2",
                      model_name = "Model~1"),
    summarize_by_columns(m2_samps$psi1) %>%
        dplyr::mutate(param = "psi1",
                      model_name = "Model~2"),
    summarize_by_columns(m2_samps$psi2) %>%
        dplyr::mutate(param = "psi2",
                      model_name = "Model~2"),
    summarize_by_columns(m1_samps$phi) %>%
        dplyr::mutate(param = "phi",
                      model_name = "Model~1"),
    summarize_by_columns(m2_samps$phi) %>%
        dplyr::mutate(param = "phi",
                      model_name = "Model~2")
)

post_prob_re <- post_prob_re %>%
    dplyr::mutate(
        post_prob_pos = n_pos / count,
        post_prob_cat = cut(
            post_prob_pos,
            breaks = c(0, .05, .2, .8, .95, 1),
            include.lowest = TRUE,
            right = FALSE,
            ordered_result = TRUE
        )
    ) %>%
    dplyr::select(c_idx, param, model_name, post_prob_pos, post_prob_cat)

rm(m1_samps, m2_samps)
gc()

## Join summaries with posterior probability
reffs <- reffs %>%
    dplyr::left_join(post_prob_re)

# reff_brks <- c(min(reffs$p500),
#                -.5, -.3, -.2, -.1, -.05, .05, .1, .2, .3, .5,
#                max(reffs$p500), Inf)

# reff_lbls <- c(sprintf("[%0.2f, %0.2f]",
#                        round(reff_brks[1:10], 2),
#                        round(reff_brks[2:11], 2)),
#                sprintf("[%0.2f, %0.2f]",
#                        round(reff_brks[11], 2),
#                        round(reff_brks[12], 2)))

reff_brks <- sort(c(exp(min(reffs$p500)),
                    c(1 / c(
                        1.05, 1.15, 1.25, 1.35, 1.5
                    )),
                    c(1.05, 1.15, 1.25, 1.35, 1.5),
                    exp(max(reffs$p500)), Inf))

reff_lbls <- sprintf("%0.2f", reff_brks[1:12])

reffs <- reffs %>%
    dplyr::mutate(
        exp_p500 = exp(p500),
        exp_p500_sig = dplyr::case_when(
            post_prob_pos > .8 ~ exp(p500),
            post_prob_pos < .2 ~ exp(p500),
            TRUE ~ NA_real_
        ),
        ep500_cat = cut(
            exp_p500,
            breaks = reff_brks,
            labels = reff_lbls,
            include.lowest = TRUE,
            dig.lab = 2,
            ordered_result = TRUE
        ),
        ep500_cat_sig = cut(
            exp_p500_sig,
            breaks = reff_brks,
            labels = reff_lbls,
            include.lowest = TRUE,
            dig.lab = 2,
            ordered_result = TRUE
        ),
        param_unicode = factor(
            param,
            levels = c("psi1", "phi", "psi2"),
            labels = c(
                "White-specific (\u03C8\u2081)",
                "Unscaled shared (\u03C6)",
                "Black-specific (\u03C8\u2082)"
            )
        ),
        eparam_unicode = factor(
            param,
            levels = c("psi1", "phi", "psi2"),
            labels = c(
                "White-specific (exp(\u03C8\u2081))",
                "Unscaled shared (exp(\u03C6))",
                "Black-specific (exp(\u03C8\u2082))"
            )
        ),
        model_plain = dplyr::case_when(
            model_name == "Model~1" ~ "Without income adjustment",
            model_name == "Model~2" ~ "Income adjusted"
        ),
        model_plain = factor(
            model_plain,
            levels = c("Without income adjustment", "Income adjusted"),
            ordered = TRUE
        )
    )

post_probab <- ggplot2::ggplot(data = reffs) +
    ggplot2::geom_map(
        ggplot2::aes(map_id = fipschar, fill = post_prob_cat),
        map = subcounties_df,
        color = "black",
        size = .05
    ) +
    ggplot2::geom_path(
        data = substates_df,
        ggplot2::aes(long, lat, group = group),
        color = "black",
        size = .2,
        alpha = .9
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = range(substates_df$long)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = range(substates_df$lat)) +
    ggplot2::coord_map("albers", lat0 = 37.5, lat1 = 29.5) +
    ggplot2::facet_grid(eparam_unicode ~ model_plain, switch = "y") +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_brewer(NULL, palette = "PRGn", direction = -1)  +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        panel.spacing.x = grid::unit(-3.75, "lines"),
        panel.spacing.y = grid::unit(-1, "lines"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::labs(title = NULL, # "Posterior probability of relative risk exceeds 1",
                  subtitle = NULL) + # "All non-white counties are 'significant'. Darker counties represent higher probability.") +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(3.5, "lines"),
            keyheight = grid::unit(.5, "lines"),
            label.hjust = 0
        )
    )
ggplot2::ggsave(
    post_probab,
    file = './plots/risk_surface_maps_prob.jpg',
    width = 7,
    height = 6,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    post_probab,
    file = './plots/risk_surface_maps_prob.pdf',
    width = 7,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)


reff_dens <-
    ggplot2::ggplot(data = reffs, ggplot2::aes(x = exp(p500), color = model_name)) +
    ggplot2::geom_density(alpha = .9) +
    ggplot2::facet_grid( ~ param_exp, labeller = ggplot2::label_parsed) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::theme_classic() +
    ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = c(.99, .99),
        legend.justification = c(1, 1)
    ) +
    ggplot2::scale_x_continuous("County median relative risk", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(NULL, expand = c(0, 0)) +
    ggplot2::labs(title = NULL) #"Distribution of county median\nrisk surface estimates")
ggplot2::ggsave(
    reff_dens,
    file = './plots/risk_surface_dens.jpg',
    width = 7,
    height = 4,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    reff_dens,
    file = './plots/risk_surface_dens.pdf',
    width = 7,
    height = 4,
    scale = 1.25
)

reff_hist <-
    ggplot2::ggplot(data = reffs, ggplot2::aes(x = ep500_cat, fill = ep500_cat)) +
    ggplot2::geom_histogram(alpha = 1,
                            stat = "count",
                            color = "black") +
    ggplot2::facet_grid(eparam_unicode ~ model_plain, switch = "y") +
    ggplot2::scale_fill_brewer(NULL, palette = "PiYG", direction = -1) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
            angle = 270,
            hjust = 0,
            vjust = .5
        )
    ) +
    ggplot2::scale_x_discrete("County median", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(NULL, expand = c(0, 0)) +
    ggplot2::labs(title = NULL) #"Distribution of exponentiated county median risk surface estimates")
ggplot2::ggsave(
    reff_hist,
    file = './plots/fig9_risk_surface_hist.jpg',
    width = 6,
    height = 6,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    reff_hist,
    file = './plots/fig9_risk_surface_hist.pdf',
    width = 6,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)


reffects <- ggplot2::ggplot(data = reffs) +
    ggplot2::geom_map(
        ggplot2::aes(map_id = fipschar, fill = ep500_cat),
        map = subcounties_df,
        color = "black",
        size = .05
    ) +
    ggplot2::geom_path(
        data = substates_df,
        ggplot2::aes(long, lat, group = group),
        color = "black",
        size = .2,
        alpha = .9
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = range(substates_df$long)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = range(substates_df$lat)) +
    ggplot2::coord_map("albers", lat0 = 37.5, lat1 = 29.5) +
    ggplot2::facet_grid(eparam_unicode ~ model_plain, switch = "y") +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_brewer("Median posterior estimate",
                               palette = "PiYG",
                               direction = -1) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        panel.spacing.x = grid::unit(-3.75, "lines"),
        panel.spacing.y = grid::unit(-1, "lines"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::labs(title = NULL) + #"Posterior median estimates of spatial surfaces") +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(3, "lines"),
            keyheight = grid::unit(.5, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )
ggplot2::ggsave(
    reffects,
    file = './plots/risk_surface_maps.jpg',
    width = 7,
    height = 6,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    reffects,
    file = './plots/risk_surface_maps.pdf',
    width = 7,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)



reffects_sig <- ggplot2::ggplot(data = reffs) +
    ggplot2::geom_map(
        ggplot2::aes(map_id = fipschar, fill = ep500_cat_sig),
        map = subcounties_df,
        color = "black",
        size = .05
    ) +
    ggplot2::geom_path(
        data = substates_df,
        ggplot2::aes(long, lat, group = group),
        color = "black",
        size = .2,
        alpha = .9
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = range(substates_df$long)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = range(substates_df$lat)) +
    ggplot2::coord_map("albers", lat0 = 37.5, lat1 = 29.5) +
    ggplot2::facet_grid(eparam_unicode ~ model_plain, switch = "y") +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_brewer(
        "Median posterior estimate",
        palette = "PiYG",
        direction = -1,
        na.value = "grey50"
    ) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text.x = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        panel.spacing.x = grid::unit(-3.75, "lines"),
        panel.spacing.y = grid::unit(-1, "lines"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::labs(title = NULL) + #"Posterior median estimates of spatial surfaces") +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(3, "lines"),
            keyheight = grid::unit(.5, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )
ggplot2::ggsave(
    reffects_sig,
    file = './plots/fig2_risk_surface_maps_sig.jpg',
    width = 7,
    height = 6,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    reffects_sig,
    file = './plots/fig2_risk_surface_maps_sig.pdf',
    width = 7,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
