library(coda)
library(rstan)
library(gmailr)
library(matrixStats)


copy_rename_stan <- function(orig_file,
                             f_time = tstamp,
                             return_new_file = TRUE) {
    ## Function that copies a file, renames it, and returns new file
    ## Extract just the file name
    base_file <- basename(orig_file)
    
    new_file <-
        paste0('./', substr(base_file, 1, nchar(base_file) - 5),
               "_", f_time, ".stan")
    
    file.copy(orig_file, new_file, overwrite = TRUE)
    
    if (return_new_file) {
        return(new_file)
    }
}


email_alert_to_myself <- function() {
    ## Uses `gmailr` to send an alert when a model has finished running.
    ## Note that you need to run the authorization for every new project
    ## on every new computer
    body_text <-
        paste0(
            "R finished running at ",
            Sys.time(),
            "<br>",
            "Model has timestamp: ",
            tstamp,
            "<br>",
            "Model name is: ",
            model_name,
            "<br>",
            "Model file is: ",
            stan_file,
            "<br>",
            "Chains / iterations / burnin / thinning / delta: ",
            n_chains,
            " / ",
            n_iter,
            " / ",
            n_burnin,
            " / ",
            n_thin,
            " / ",
            a_delta
        )
    subject_text <- paste0("Model ", model_name, " (", tstamp,
                           ") has finished running!")
    # sink("/dev/null/")
    gmailr::mime() %>%
        gmailr::to("mathewwithonet+ralerts@gmail.com") %>%
        gmailr::from("mathewwithonet@gmail.com") %>%
        gmailr::subject(print(subject_text)) %>%
        gmailr::html_body(print(body_text)) %>%
        gmailr::send_message()
    # sink()
}


get_random_seed <-
    function(seed_file = paste0('./seed_', tstamp, '.rds')) {
        ## Generates and saves a random seed (with the model timestamp as the name)
        ## so that we can keep track of the seeds we use.
        if (file.exists(seed_file)) {
            random_seed <- readRDS(seed_file)
        } else {
            random_seed <- sample(.Machine$integer.max, 1)
            saveRDS(random_seed, file = seed_file)
        }
        return(random_seed)
    }


save_sessioninfo <- function(file_name =
                                 paste0('./sessionInfo_', tstamp, '.txt')) {
    ## Print out the session information after making a file -- useful for
    ## future debugging if results seem strange we can get the versions for
    ## all the packages
    if (!file.exists(file_name)) {
        sink(file = file_name, type = "output")
        print(utils::sessionInfo())
        sink()
    }
}


summarize_stanfit <- function(stanfit,
                              pars = c("alpha",
                                       "beta",
                                       "delta",
                                       "nu",
                                       "psi",
                                       "phi",
                                       "sigma_u",
                                       "sigma_v",
                                       "state_s")) {
    ## Just a wrapper to get stanfit summary as a dataframe with better
    ## column names and for parameters we care about.
    df <-
        as.data.frame(rstan::summary(stanfit, pars = pars)$summary)
    names(df)[4:8] <- c("p025", "p250", "p500", "p750", "p975")
    return(df)
}
