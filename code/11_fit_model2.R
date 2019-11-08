## Imports ----
library(rstan)
library(gmailr)
library(tidyverse)
source('./code/helpers/stan_helpers.R')
source('./code/helpers/misc_helpers.R')

## Set up auto-email auth ----
gmail_auth()

## RStan options
## bug doesn't respect chain_id when auto_write == TRUE
## see: https://github.com/stan-dev/rstan/issues/294
# rstan_options(auto_write = FALSE)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Modeling parameters ----
##  Naming
tstamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
mkdir_p(paste0('./model_run_', tstamp))
model_name <- 'm1s_cp_reg_100k_8c_40t_995d'
model_file <- 'm1s_cp_reg_100k_8c_40t_995d'
stan_file <- './stan_code/m1s_with_income.stan'
random_seed <- get_random_seed(paste0('./model_run_', tstamp,
                                      '/seed_', tstamp, '.rds'))

##  Run parameters
n_chains <- 8
n_iter <- 100000
n_burnin <- floor(n_iter / 2)
n_thin <- 40
verbose_flag <- FALSE
dont_save_pars = c("v_unstr", "u_str_unscaled", "u_str")

##  Search
a_delta = .995  # default = .8
t_depth = 35    # max tree depth, default = 10

## Load data ----
prem <- readRDS('./data_working/premature_working_data.RDS')
sparse_A <- readRDS('./data_working/county_adj_sparse_parts.RDS')

## Mean center income and change to per $10,000 ----
income <- (prem$med_hh_income -
               mean(prem$med_hh_income, na.rm = TRUE)) / 10000
income[is.na(income)] <-
    0      # 4 counties don't have income -- assign mean

## Get the data in order ----
pre_df  <-
    list(
        m = nrow(prem),
        # number of observations
        s = length(unique(prem$s_idx)),
        # number of states
        n = length(sparse_A$D_sparse),
        # number of areas
        y = prem$observed,
        # vector of observed (int)
        log_offset = log(prem$expected),
        # log of expected
        c_idx = prem$c_idx,
        # county index
        s_idx = prem$s_idx,
        # state index
        
        d1_idx = 1 - prem$black,
        # {1, 0} vector for dis_1
        d2_idx = prem$black,
        # {0, 1} vector for dis_2
        
        income = income,
        
        # Use return_sparse_parts(A) for these next ones
        D_sparse = sparse_A$D_sparse,
        # neighbors per node
        W_sparse = sparse_A$W_sparse,
        # adjacent pairs
        lambda = sparse_A$lambdas,
        # eigenvalues
        W_n = nrow(sparse_A$W_sparse)
    )     # number of edges

## Save the passed data in case we need it later ----
saveRDS(pre_df,
        file = paste0('./model_run_', tstamp,
                      '/passed_data_', tstamp, '.rds'))

## Stan ----
new_stan_file <- copy_rename_stan(stan_file)
premature_fit <- stan(
    file = new_stan_file,
    model_name = model_name,
    data = pre_df,
    thin = n_thin,
    iter = n_iter,
    warmup = n_burnin,
    chains = n_chains,
    verbose = verbose_flag,
    pars = dont_save_pars,
    include = FALSE,
    save_dso = TRUE,
    seed = random_seed,
    control = list(adapt_delta = a_delta,
                   max_treedepth = t_depth),
    refresh = n_iter / 100,
    sample_file = paste0('./model_run_', tstamp,
                         '/sample_file')
)

## Move stan file into model_run ----
file.rename(from = new_stan_file,
            to = paste0('./model_run_', tstamp, '/',
                        substr(new_stan_file, 3, nchar(new_stan_file))))

## Remove the compiled stan model ----
file.remove(paste0(substr(new_stan_file, 1, nchar(new_stan_file) - 4), 'rds'))

## Save summary and fit objects ----
mkdir_p('./stanfit_objects/')
mkdir_p('./stanfit_summaries/')

saveRDS(premature_fit,  './stanfit_objects/model1_with_income.RDS')

prem_summary <- summarize_stanfit(
    premature_fit,
    pars = c(
        "alpha",
        "beta",
        "delta",
        "psi",
        "phi",
        "nu",
        "sigma_u",
        "sigma_v",
        "sigma_s"
    )
)
saveRDS(prem_summary,
        './stanfit_summaries/model1_with_income_summ.RDS')

## Wrap up ----
save_sessioninfo(file_name = paste0('./model_run_', tstamp, '/',
                                    "sessionInfo.txt"))
email_alert_to_myself()
