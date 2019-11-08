## Imports ----
library(tidyverse)
library(viridis)

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
sav_folder <- cfig$data_dir

## Load age standardized rates ----
std_rates <-
    readRDS(sprintf("%s/age_std_rates_by_race.RDS", sav_folder)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        race_cat = ifelse(black == 1, "Non-Hispanic Black",
                          "Non-Hispanic White"),
        death_rate = ifelse(total_deaths < 10, NA, death_rate),
        death_rate_orig = death_rate,
        death_rate = dplyr::case_when(
            death_rate >= stats::quantile(death_rate, .9975, na.rm = TRUE) ~
                stats::quantile(death_rate, .9975, na.rm = TRUE),
            death_rate <= stats::quantile(death_rate, .0025, na.rm = TRUE) ~
                stats::quantile(death_rate, .0025, na.rm = TRUE),
            TRUE ~ death_rate
        ),
        log_rate = log(death_rate),
        trunc_rate = dplyr::case_when(
            death_rate_orig <= 150 ~ 150,
            death_rate_orig >= 450 ~ 450,
            TRUE ~ death_rate_orig
        )
    )

## Load map files ----
load(sprintf("%s/lower_us_maps.RData", sav_folder))

## Make breaks and labels ----
brks <- log(c(
    min(std_rates$death_rate, na.rm = TRUE),
    150,
    200,
    250,
    300,
    350,
    400,
    450,
    500,
    550,
    600,
    max(std_rates$death_rate, na.rm = TRUE)
))

lbls <- sprintf("%i",
                round(c(
                    min(std_rates$death_rate, na.rm = TRUE),
                    150,
                    200,
                    250,
                    300,
                    350,
                    400,
                    450,
                    500,
                    550,
                    600,
                    max(std_rates$death_rate, na.rm = TRUE)
                )))

lbls[c(3, 5, 7, 9, 11)] <- ""

## Plot age-standardized rates:
std_rates_plot <-
    ggplot2::ggplot(data = std_rates, ggplot2::aes(group = race_cat)) +
    ggplot2::geom_map(ggplot2::aes(map_id = fipschar, fill = log_rate),
                      map = subcounties_df) +
    ggplot2::geom_path(
        data = substates_df,
        ggplot2::aes(long, lat, group = group),
        color = "black",
        size = .2,
        alpha = .9
    ) +
    ggplot2::coord_map("albers", lat0 = 37.5, lat1 = 29.5) +
    viridis::scale_fill_viridis(
        "Age-standardized premature mortality (per 100,000)",
        option = "viridis",
        direction = -1,
        discrete = FALSE,
        guide = ggplot2::guide_colorbar(
            nrow = 1,
            label.position = "bottom",
            title.position = "top"
        ),
        breaks = brks,
        labels = lbls
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = range(subcounties_df$long)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = range(subcounties_df$lat)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.key.width = grid::unit(2, "cm"),
        legend.key.height = grid::unit(.25, "cm"),
        panel.spacing.x = grid::unit(-3.75, "lines"),
        panel.spacing.y = grid::unit(-1, "lines"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::facet_grid(race_cat ~ .) +
    ggplot2::labs(
        x = NULL,
        y = NULL,
        title = NULL,
        #"Age-standardized premature mortality rate",
        subtitle =  NULL,
        #"By race/ethnicity, 2010-2015",
        caption = NULL
    )

saveRDS(std_rates_plot, "./grobs/fig1_agestd_rates.RDS")

ggplot2::ggsave(
    filename = "./plots/fig0_standard_rates_premature.jpg",
    std_rates_plot,
    width = 6,
    height = 8,
    dpi = 300,
    scale = 1.25
)

ggplot2::ggsave(
    filename = "./plots/fig0_standard_rates_premature.pdf",
    std_rates_plot,
    width = 6,
    height = 8,
    device = grDevices::cairo_pdf,
    scale = 1.25
)


std_rates_plot_t <-
    ggplot2::ggplot(data = std_rates, ggplot2::aes(group = race_cat)) +
    ggplot2::geom_map(ggplot2::aes(map_id = fipschar, fill = trunc_rate),
                      map = subcounties_df) +
    ggplot2::geom_path(
        data = substates_df,
        ggplot2::aes(long, lat, group = group),
        color = "black",
        size = .1,
        alpha = .5
    ) +
    ggplot2::coord_map("albers", lat0 = 37.5, lat1 = 29.5) +
    viridis::scale_fill_viridis(
        "Premature mortality (per 100,000)",
        option = "magma",
        direction = -1,
        discrete = FALSE,
        guide = ggplot2::guide_colorbar(
            nrow = 1,
            label.position = "bottom",
            title.position = "top"
        ),
        breaks = c(150, 250, 350, 450),
        labels = c(
            sprintf("%0.1f to 150",
                    min(std_rates$death_rate_orig,
                        na.rm = TRUE)),
            "250",
            "350",
            sprintf("450 to %0.1f",
                    max(std_rates$death_rate_orig,
                        na.rm = TRUE))
        )
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = range(subcounties_df$long)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = range(subcounties_df$lat)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.key.width = grid::unit(2, "cm"),
        legend.key.height = grid::unit(.25, "cm"),
        panel.spacing.x = grid::unit(-6.75, "lines"),
        panel.spacing.y = grid::unit(-1, "lines"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    ) +
    ggplot2::facet_grid(race_cat ~ ., switch = "y") +
    ggplot2::labs(
        x = NULL,
        y = NULL,
        title = NULL,
        #"Age-standardized premature mortality rate",
        subtitle = NULL,
        #"By race/ethnicity, 2010-2015",
        caption = NULL
    )# paste0("Counties with fewer than 10 deaths are not plotted."))

saveRDS(std_rates_plot_t, "./grobs/fig7_agestd_rates_trunc.RDS")

ggplot2::ggsave(
    filename = "./plots/fig7_standard_rates_premature_trunc.jpg",
    std_rates_plot_t,
    width = 6,
    height = 6.5,
    dpi = 300,
    scale = 1.25
)

ggplot2::ggsave(
    filename = "./plots/fig7_standard_rates_premature_trunc.pdf",
    std_rates_plot_t,
    width = 6,
    height = 6.5,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
