## Imports ----
library(tidyverse)
library(viridis)
library(sp)

## Load the data ----
load('./data_working/lower_us_maps.RData')
prem <- readRDS('./data_working/premature_working_data.RDS')

## Double copy of data so just take one copy ----
prem <- prem %>%
    dplyr::group_by(fipschar, county_name) %>%
    dplyr::summarize(income = mean(med_hh_income))


income_map <- ggplot2::ggplot(data = prem) +
    ggplot2::geom_map(ggplot2::aes(map_id = fipschar, fill = log(income)),
                      map = subcounties_df) +
    ggplot2::geom_path(
        data = substates_df,
        ggplot2::aes(long, lat, group = group),
        color = "white",
        size = .2,
        alpha = .5
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = range(subcounties_df$long)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = range(subcounties_df$lat)) +
    ggplot2::coord_map("albers", lat0 = 37.5, lat1 = 29.5) +
    ggplot2::theme_classic() +
    viridis::scale_fill_viridis(
        "Median household income ($1,000)",
        option = "viridis",
        direction = -1,
        breaks = c(log(c(
            min(prem$income, na.rm = TRUE),
            25000,
            50000,
            75000,
            max(prem$income, na.rm = TRUE)
        ))),
        labels = function(x)
            sprintf(fmt = "%0.1f", round(exp(x) / 1000, 1))
    ) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        strip.text.x = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.spacing.x = grid::unit(-1.45, "cm"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom"
    ) +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::guides(
        fill = ggplot2::guide_colorbar(
            label.position = "bottom",
            barwidth = grid::unit(10, "cm"),
            barheight = grid::unit(.4, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )

ggplot2::ggsave(
    './plots/income_map.pdf',
    income_map,
    width = 5,
    height = 3.5,
    scale = 1.25
)
ggplot2::ggsave(
    './plots/income_map.jpg',
    income_map,
    width = 5,
    height = 3.5,
    scale = 1.25,
    dpi = 300
)
