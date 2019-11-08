## Imports ----
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(sp)
library(cowplot)
source('./code/helpers/99_helper_functions.R')

## Load the data ----
load('./data_working/lower_us_maps.RData')
load("./data_working/residual_disparities_data.RData")
fips_idx <- readRDS('./data_working/fips_to_dummy_mappings.RDS')

## Need a copy of the model column ----
## so we can plot double lines in histogram
resid_df <- resid_df %>%
    dplyr::mutate(
        model_n = model_name,
        disp_type2 = factor(
            disp_type,
            levels = c("residual", "total"),
            labels = c("Spatially-varying\nDisparity",
                       "Relative Risk"),
            ordered = TRUE
        )
    ) %>%
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
    dplyr::mutate(model_name = factor(
        model_n,
        levels = c("Model 1", "Model 2"),
        labels = c("No income adjustment",
                   "Income-adjusted"),
        ordered = TRUE
    ))

resid_df_sig <- resid_df %>%
    dplyr::mutate(p500 = ifelse(sig == 1, p500, NA))

reff_brks <- c(min(resid_df$p500),
               c(1 / c(1.05, 1.15, 1.25, 1.35, 1.5)),
               c(1.05, 1.15, 1.25, 1.35, 1.5),
               max(resid_df$p500),
               Inf)

## Plot densities ----
disp_dens <- ggplot2::ggplot(
    resid_df %>%
        dplyr::filter(disp_type2 == "Relative Risk"),
    ggplot2::aes(x = exp(p500), color = model_name)
) +
    ggplot2::geom_density(alpha = .9) +
    ggplot2::scale_color_brewer(NULL, palette = "Set1") +
    ggplot2::theme_classic() +
    ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1)
    ) +
    ggplot2::scale_x_continuous("Relative Risk (black/white)", expand = c(0, 0.1)) +
    ggplot2::scale_y_continuous(NULL, expand = c(0, 0.005)) +
    ggplot2::labs(title = NULL)
ggplot2::ggsave(
    './plots/fig4_rel_risk_density.pdf',
    disp_dens,
    height = 3,
    width = 6,
    scale = 1.25
)
saveRDS(disp_dens, "./grobs/rel_risk_density.RDS")

## Discretized disparities
cut_pts <- c(.05, .1, .2, .4, .5)
brks <- c(min(resid_df$p500),
          sort(log(c(
              1 - cut_pts, 1 + cut_pts
          ))),
          max(resid_df$p500), Inf)
lbls <- sprintf("%0.2f", round(exp(brks)[1:12], 2))

resid_df <- resid_df %>%
    dplyr::mutate(
        p500_sig = ifelse(sig == 1, p500, NA),
        p500_cat = cut(
            p500,
            breaks = brks,
            labels = lbls,
            include.lowest = TRUE,
            ordered_result = TRUE
        ),
        p500_cat_sig = cut(
            p500_sig,
            breaks = brks,
            labels = lbls,
            include.lowest = TRUE,
            ordered_result = TRUE
        ),
        model_name = factor(
            model_n,
            levels = c("Model 1", "Model 2"),
            labels = c("No income adjustment",
                       "Income-adjusted"),
            ordered = TRUE
        )
    )

disp_map <- ggplot2::ggplot(data = resid_df) +
    ggplot2::geom_map(
        ggplot2::aes(map_id = fipschar, fill = p500_cat),
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
    ggplot2::facet_grid(model_name ~ ., switch = "y") +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_brewer("Relative Risk (black/white)",
                               palette = "Spectral",
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
    ggplot2::labs(title = NULL) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(2.25, "lines"),
            keyheight = grid::unit(.4, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )
ggplot2::ggsave(
    disp_map,
    file = './plots/fig3c_relative_risk_maps_categorized.jpg',
    width = 6,
    height = 7,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    disp_map,
    file = './plots/fig3c_relative_risk_maps_categorized.pdf',
    width = 6,
    height = 7,
    scale = 1.25,
    device = grDevices::cairo_pdf
)

disp_prob <- ggplot2::ggplot(data = resid_df) +
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
    ggplot2::facet_grid(model_name ~ ., switch = "y") +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_brewer("Posterior probability disparity > 1",
                               palette = "PRGn",
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
        panel.background = ggplot2::element_rect(fill = "transparent",
                                                 color = NA)
    ) +
    ggplot2::labs(title = NULL) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(2.25, "lines"),
            keyheight = grid::unit(.4, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )
ggplot2::ggsave(
    disp_prob,
    file = './plots/fig3b_relative_risk_maps_prob.jpg',
    width = 6,
    height = 7,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    disp_prob,
    file = './plots/fig3b_relative_risk_maps_prob.pdf',
    width = 6,
    height = 7,
    scale = 1.25,
    device = grDevices::cairo_pdf
)

disp_map_sig <- ggplot2::ggplot(data = resid_df) +
    ggplot2::geom_map(
        ggplot2::aes(map_id = fipschar, fill = p500_cat_sig),
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
    ggplot2::facet_grid(model_name ~ ., switch = "y") +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_brewer(
        "Relative Risk (black/white)",
        palette = "Spectral",
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
    ggplot2::labs(title = NULL) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(2.25, "lines"),
            keyheight = grid::unit(.4, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )
ggplot2::ggsave(
    disp_map_sig,
    file = './plots/fig3_relative_risk_maps_categorized_sig.jpg',
    width = 6,
    height = 7,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    disp_map_sig,
    file = './plots/fig3_relative_risk_maps_categorized_sig.pdf',
    width = 6,
    height = 7,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
