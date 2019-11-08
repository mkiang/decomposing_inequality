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
prem <- readRDS("./data_working/premature_working_data.RDS") %>%
    dplyr::mutate(
        exp_p500 = observed / expected,
        param = dplyr::case_when(black == 0 ~ "white_smr",
                                 black == 1 ~ "black_smr"),
        model_name = "raw",
        p500 = NA
    )

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


## Now calculate the county SMR
smr_df <- rbind(
    summarize_by_columns(
        m1_samps$alpha1 + m1_samps$psi1 +
            (m1_samps$delta * m1_samps$phi) +
            nu_expanded_1
    ) %>%
        dplyr::mutate(param = "white_smr",
                      model_name = "Unadjusted"),
    summarize_by_columns(
        m1_samps$alpha2 + m1_samps$psi2 +
            (m1_samps$phi / m1_samps$delta) +
            nu_expanded_1
    ) %>%
        dplyr::mutate(param = "black_smr",
                      model_name = "Unadjusted"),
    summarize_by_columns(
        m2_samps$alpha1 + m2_samps$psi1 +
            (m2_samps$delta * m2_samps$phi) +
            nu_expanded_2
    ) %>%
        dplyr::mutate(param = "white_smr",
                      model_name = "Adjusted"),
    summarize_by_columns(
        m2_samps$alpha2 + m2_samps$psi2 +
            (m2_samps$phi / m2_samps$delta) +
            nu_expanded_2
    ) %>%
        dplyr::mutate(param = "black_smr",
                      model_name = "Adjusted")
) %>%
    dplyr::left_join(fips_idx_map)

## Bind with the observed SMR
new_df <- rbind(
    smr_df %>%
        dplyr::select(c_idx, fipschar, param, model_name, p500) %>%
        dplyr::mutate(exp_p500 = exp(p500)),
    prem %>%
        dplyr::select(c_idx, fipschar, param, model_name, exp_p500, p500)
)

##  Save the modeled and raw SMRs into a dataframe
saveRDS(new_df, "./data_working/modeled_and_raw_smrs.RDS")

## Categorize SMR
smr_brks <- sort(c(
    min(new_df$exp_p500),
    c(1 / c(1.05, 1.15, 1.25, 1.35, 1.5)),
    c(1.05, 1.15, 1.25, 1.35, 1.5),
    max(new_df$exp_p500),
    Inf
))
smr_lbls <- sprintf("%0.2f", smr_brks[1:12])


new_df <- new_df %>%
    dplyr::mutate(
        ep500_cat = cut(
            exp_p500,
            breaks = smr_brks,
            labels = smr_lbls,
            include.lowest = TRUE,
            dig.lab = 2,
            ordered_result = TRUE
        ),
        model = factor(
            model_name,
            levels = c("raw", "Unadjusted", "Adjusted"),
            labels = c("Raw", "Without income adjustment", "Income-adjusted"),
            ordered = TRUE
        ),
        race = factor(
            param,
            levels = c("white_smr", "black_smr"),
            labels = c("Non-Hispanic White",
                       "Non-Hispanic Black"),
            ordered = TRUE
        )
    )

smr_maps <- ggplot2::ggplot(data = new_df) +
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
    ggplot2::facet_grid(model ~ race, switch = "y") +
    ggplot2::theme_classic() +
    viridis::scale_fill_viridis(
        "Standardized mortality ratio",
        option = "plasma",
        direction = -1,
        discrete = TRUE
    )  +
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
    ggplot2::labs(title = NULL,
                  subtitle = NULL) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            nrow = 1,
            label.position = "bottom",
            keywidth = grid::unit(3.5, "lines"),
            keyheight = grid::unit(.5, "lines"),
            label.hjust = 0,
            title.position = "top"
        )
    )
ggplot2::ggsave(
    smr_maps,
    file = './plots/fig1_smr_maps.jpg',
    width = 7,
    height = 6,
    scale = 1.25,
    dpi = 300
)
ggplot2::ggsave(
    smr_maps,
    file = './plots/fig1_smr_maps.pdf',
    width = 7,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
