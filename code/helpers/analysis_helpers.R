library(tidyverse)
library(rstan)


reshape_reffects <- function(stan_summary) {
    ## Given a stanfit summary, extract the random effects in a dataframe we
    ## can quickly plot in ggplot
    psi1 <- stan_summary[grep(pattern = 'psi[1,',
                              x = rownames(stan_summary),
                              fixed = TRUE),]
    psi2 <- stan_summary[grep(pattern = 'psi[2,',
                              x = rownames(stan_summary),
                              fixed = TRUE),]
    phi  <- stan_summary[grep(pattern = 'phi[',
                              x = rownames(stan_summary),
                              fixed = TRUE),]
    
    ## Extract the c_idx from row_names for each
    ## Add param name
    psi1$c_idx <- as.numeric(unlist(lapply(
        strsplit(unlist(lapply(
            strsplit(rownames(psi1), ","),
            FUN = function(x)
                x[2]
        )), "]"),
        FUN = function(x)
            x[1]
    )))
    psi1$param <- "psi1"
    
    psi2$c_idx <- as.numeric(unlist(lapply(
        strsplit(unlist(lapply(
            strsplit(rownames(psi2), ","),
            FUN = function(x)
                x[2]
        )), "]"),
        FUN = function(x)
            x[1]
    )))
    psi2$param <- "psi2"
    
    phi$c_idx <- as.numeric(unlist(strsplit(unlist(
        lapply(
            strsplit(rownames(phi), "[", fixed = TRUE),
            FUN = function(x)
                x[2]
        )
    ), ']')))
    phi$param <- "phi"
    
    ## Remove the old rownames
    rownames(psi1) <- rownames(psi2) <- rownames(phi) <- NULL
    
    ## Combine
    random_effects <- rbind(psi1, psi2, phi)
    
    ## Make parseable label values for plotting
    random_effects$param_exp <- factor(
        random_effects$param,
        levels = c("psi1", "phi", "psi2"),
        labels = c(
            "White-specific~risk~(psi[1][i])",
            "Unscaled~shared~risk~(phi[i])",
            "Black-specific~risk~(psi[2][i])"
        ),
        ordered = TRUE
    )
    
    return(random_effects)
}


reshape_seffects <- function(stan_summary) {
    ## Given a stanfit summary, extract the state effects in a dataframe we
    ## can quickly plot in ggplot
    nu <- stan_summary[grep(pattern = 'nu[',
                            x = rownames(stan_summary),
                            fixed = TRUE),]
    
    ## Extract the s_idx from row_names for each
    ## Add param name
    nu$s_idx <- as.numeric(unlist(strsplit(unlist(
        lapply(
            strsplit(rownames(nu), "[", fixed = TRUE),
            FUN = function(x)
                x[2]
        )
    ), ']')))
    nu$param <- "nu"
    
    nu$param_exp <- factor(
        nu$param,
        levels = c("nu"),
        labels = c("State effect"),
        ordered = TRUE
    )
    
    ## Remove the old rownames
    rownames(nu) <- NULL
    
    return(nu)
}


extract_samples <- function(stanfit) {
    ## Extract samples from a stanfit object and return a list of variables
    ## NOTE: Random effects will be matrices of dimensions iterations x areas
    ##       while the fixed effects will just be columns of length iterations
    
    ## Extract the samples of fixed effects from stan_fit object
    fixed_pars <- stanfit@model_pars[grepl(stanfit@model_pars,
                                           pattern = "alpha|beta|delta|sigma")]
    x <- as.matrix(stanfit, pars = fixed_pars)
    
    ## Get a list of the names and make them prettier
    var_names <-
        gsub(x = colnames(x),
             pattern = "\\[|\\]",
             replacement = "")
    
    ## Turn the named matrix into a list of vectors
    x <- split(x, c(col(x)))
    
    ## Put back in the variable names
    names(x) <- var_names
    
    ## Now extract the random effects and attach the random effects
    random_pars <- stanfit@model_pars[grepl(stanfit@model_pars,
                                            pattern = "nu|psi|phi")]
    r_effs <- as.matrix(stanfit, pars = random_pars)
    x$psi1 <- r_effs[, grep(x = colnames(r_effs),
                            pattern = 'psi[1',
                            fixed = TRUE)]
    x$psi2 <- r_effs[, grep(x = colnames(r_effs),
                            pattern = 'psi[2',
                            fixed = TRUE)]
    x$phi  <- r_effs[, grep(x = colnames(r_effs),
                            pattern = 'phi',
                            fixed = TRUE)]
    x$nu   <- r_effs[, grep(x = colnames(r_effs),
                            pattern = 'nu',
                            fixed = TRUE)]
    
    return(x)
}


calc_residual_disparity <-
    function(extracted_samples, expo = FALSE) {
        ## Residual disparity, disparities not including the baseline (alphas):
        ##  log residual disparity = phi * (1/delta - delta) + psi2 - psi1
        res_disp <- extracted_samples$phi *
            (1 / extracted_samples$delta - extracted_samples$delta) +
            extracted_samples$psi2 - extracted_samples$psi1
        
        res_disp <- unname(res_disp)
        
        if (expo) {
            res_disp <- exp(res_disp)
        }
        return(res_disp)
    }


sub_sample <- function(stanfit, n, keep_warmup = FALSE) {
    ## Subsamples a stanfit object -- useful when the object is too large
    ## for fast analysis. N is the number of post-warmup iterations you want
    ## to keep. This is the same as if you were to thin out a sample.
    sim <- stanfit@sim
    samp <- sim$samples
    W <- sim$warmup
    I <- sim$iter
    sel <- c(if (keep_warmup)
        1:W, sample((W + 1):I, size = n))
    
    subsamp <- lapply(samp,
                      function(chain_samp) {
                          lapply(chain_samp, function(x)
                              x[sel])
                      })
    
    stanfit@sim$samples <- subsamp
    
    return(stanfit)
}


summarize_stanfit_file <- function(RDSfile,
                                   pars = c("alpha", "beta", "delta", "psi",
                                            "phi", "sigma_u", "sigma_v")) {
    ## Just a wrapper to quickly load a stanfit object, summarize it, then
    ## remove it from the workspace
    x <- readRDS(RDSfile)
    summ_x <- summarize_stanfit(x, pars = pars)
    rm(x)
    return(summ_x)
}


reshape_summs_plotting <-
    function(combined_summaries,
             fixed_effects = TRUE,
             cname = "iter",
             buffer = .15,
             stepsize = 1) {
        ## Takes a bunch of summaries appended together and puts them into a shape
        ## that can be more easily plotted.
        unique_c <- unique(combined_summaries[[cname]])
        if (fixed_effects) {
            summs <- combined_summaries[grep(combined_summaries$pars,
                                             pattern = "alpha|beta|delta"),]
            summs$y <- rep(1:(nrow(summs) / length(unique_c)),
                           length(unique_c))
            for (i in seq_along(unique_c)) {
                summs$y[summs[cname] == unique_c[i]] <-
                    summs$y[summs[cname] == unique_c[i]] +
                    (buffer * (i - 1))
            }
        } else {
            summs <- combined_summaries
            summs$y <-
                rep(1:(nrow(summs) / length(unique_c) / 3) * stepsize,
                    length(unique_c) * 3)
            for (i in seq_along(unique_c)) {
                for (p in unique(summs$param)) {
                    summs$y[summs[cname] == unique_c[i] & summs$param == p] <-
                        summs$y[summs[cname] == unique_c[i] &
                                    summs$param == p] +
                        (buffer * (i - 1))
                }
            }
        }
        return(summs)
    }


plot_covariate_map <- function(full_acs_data, formula) {
    full_acs_data$val[is.nan(full_acs_data$val)] <- 0
    full_acs_data$race_cat <- factor(
        full_acs_data$black,
        levels = 0:1,
        labels = c("Non-Hispanic White",
                   "Non-Hispanic Black"),
        ordered = TRUE
    )
    
    df <- filter_(full_acs_data, formula)
    
    m_plot <- ggplot(data = df) +
        geom_map(aes(map_id = fipschar, fill = val), map = subcounties_df) +
        geom_path(
            data = substates_df,
            aes(long, lat, group = group),
            color = "white",
            size = .2,
            alpha = .5
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        expand_limits(x = subcounties_df$long, y = subcounties_df$lat) +
        coord_map("albers", lat0 = 39, lat1 = 45) +
        theme_classic() +
        scale_fill_viridis("", direction = -1) +
        theme(
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 12),
            strip.text.x = element_text(size = 14),
            strip.background = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(.15, "cm")
        )
    
    return(m_plot)
}


plot_covariate_dens <- function(full_acs_data, formula) {
    full_acs_data$val[is.nan(full_acs_data$val)] <- 0
    full_acs_data$race_cat <- factor(
        full_acs_data$black,
        levels = 0:1,
        labels = c("non-Hispanic White",
                   "non-Hispanic Black"),
        ordered = TRUE
    )
    
    df <- filter_(full_acs_data, formula)
    
    d_plot <- ggplot(data = df,
                     aes(
                         x = val,
                         color = race_cat,
                         group = race_cat
                     )) +
        geom_density(
            data = select(df, -race_cat),
            aes(group = black),
            color = "grey75",
            alpha = .25,
            size = .25
        ) +
        geom_density(size = 1, color = "black") +
        theme_classic() +
        scale_x_continuous(expand = c(.01, 0)) +
        scale_y_continuous(expand = c(.005, 0)) +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 18),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank()
        )
    
    return(d_plot)
}


plot_disparities_grob <- function(m1m2_disparities,
                                  cut_point = 1,
                                  with_density = TRUE,
                                  ptitle = NA,
                                  pcaption = NA) {
    if (cut_point != 1) {
        percent_to_plot <- cut_point
        lower_bound <-
            quantile(m1m2_disparities$avg, (1 - percent_to_plot) / 2,
                     na.rm = TRUE)
        upper_bound <-
            quantile(m1m2_disparities$avg,
                     percent_to_plot +
                         (1 - percent_to_plot) / 2,
                     na.rm = TRUE)
        
        m1m2_disparities$avg[m1m2_disparities$avg <= lower_bound] <-
            lower_bound
        m1m2_disparities$avg[m1m2_disparities$avg >= upper_bound] <-
            upper_bound
    }
    
    disp_map <- ggplot(data = m1m2_disparities) +
        geom_map(aes(map_id = fipschar, fill = avg), map = subcounties_df) +
        geom_path(
            data = substates_df,
            aes(long, lat, group = group),
            color = "white",
            size = .2,
            alpha = .5
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        expand_limits(x = subcounties_df$long, y = subcounties_df$lat) +
        coord_map("albers", lat0 = 39, lat1 = 45) +
        theme_classic() +
        scale_fill_viridis(
            "",
            option = "magma",
            direction = -1,
            labels = function(x)
                sprintf(fmt = "%2.2f", exp(x))
        ) +
        theme(
            plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 12),
            strip.text.x = element_text(size = 14),
            strip.background = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(.15, "cm"),
            panel.spacing.x = unit(-1.45, "cm")
        )
    
    if (!is.na(ptitle)) {
        disp_map <- disp_map + labs(title = ptitle)
    }
    
    if (with_density) {
        disp_map <- disp_map + facet_wrap( ~ model_name, nrow = 1)
        disp_den <- ggplot(data = m1m2_disparities,
                           aes(
                               x = avg,
                               color = model_name,
                               group = model_name
                           )) +
            geom_density(
                data = select(m1m2_disparities, -model_name),
                aes(group = model_n),
                color = "grey75",
                alpha = .25,
                size = .25
            ) +
            geom_density(size = 1, color = "black") +
            theme_classic() +
            facet_wrap( ~ model_name, nrow = 1) +
            scale_x_continuous(
                expand = c(.01, 0),
                labels = function(x)
                    sprintf(fmt = "%2.2f",
                            exp(x))
            ) +
            scale_y_continuous(expand = c(.005, 0)) +
            theme(
                plot.title = element_text(size = 20, face = "bold"),
                plot.subtitle = element_text(size = 18),
                axis.line = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title = element_blank(),
                legend.position = "none",
                strip.background = element_blank(),
                strip.text = element_blank()
            )
        
        if (!is.na(pcaption)) {
            disp_den <- disp_den + labs(caption = pcaption)
        }
        
        full_disp <- arrangeGrob(disp_map,
                                 disp_den,
                                 nrow = 2,
                                 layout_matrix = matrix(c(rep(1, 7),
                                                          rep(2, 2))))
    } else {
        full_disp <- disp_map
    }
    
    return(full_disp)
}

summarize_by_columns <- function(samps_matrix) {
    x <- data_frame(
        c_idx = 1:ncol(samps_matrix),
        avg   = colMeans(samps_matrix),
        p500  = colMedians(samps_matrix),
        p025  = colQuantiles(samps_matrix, probs = .025),
        p975  = colQuantiles(samps_matrix, probs = .975),
        sd    = colSds(samps_matrix),
        count = apply(
            samps_matrix,
            MARGIN = 2,
            FUN = function(x)
                sum(!is.na(x))
        ),
        n_pos = apply(
            samps_matrix,
            MARGIN = 2,
            FUN = function(x)
                sum(x >= 0)
        ),
        n_neg = apply(
            samps_matrix,
            MARGIN = 2,
            FUN = function(x)
                sum(x < 0)
        )
    )
    return(x)
}
