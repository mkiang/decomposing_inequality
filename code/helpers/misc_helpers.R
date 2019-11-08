library(tidyverse)
library(rstan)
library(coda)

mkdir_p <- function(dir_name) {
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
}

winbugs_log_to_df <- function(log_file) {
    ## Converts a WinBUGS log file into a dataframe using grep to find
    ## the start and end lines.
    raw_log <- readLines(log_file)
    skip_lines <- which(raw_log == "Node statistics")
    ending_line <- which(raw_log == "dic.stats()")
    n_records <-
        ending_line - skip_lines - 2   # 2 for the lines themselves
    
    df_log <- readr::read_delim(
        log_file,
        delim = "\t",
        escape_double = FALSE,
        col_types = readr::cols(
            X1 = readr::col_skip(),
            start = readr::col_skip(),
            sample = readr::col_skip()
        ),
        trim_ws = TRUE,
        skip = skip_lines,
        n_max = n_records
    )
    names(df_log) <- c("node", "mean", "sd", "mc_error",
                       "p025", "median", "p975")
    
    return(df_log)
}

stan_to_coda_obj <- function(stanfit) {
    ## Convert a stanfit object to a coda object
    coda::mcmc.list(lapply(1:ncol(stanfit),
                           function(x)
                               coda::mcmc(as.array(stanfit)[, x, ])))
}

n_connected_components <- function(W_sparse) {
    ## Checks the number of connected components. Should be 1.
    ## Just use make_sparse_parts() to get W_sparse from adjacency file.
    edgelist <- NULL
    for (i in 1:nrow(W_sparse)) {
        edgelist <- c(edgelist,
                      sparse_A$W_sparse[i, 1], sparse_A$W_sparse[i, 2])
    }
    g <- make_graph(edges = edgelist, directed = FALSE)
    return(components(g)$no)
}

trim_stan_csv <- function(input_file,
                          output_file,
                          thin = 1,
                          verbose = TRUE,
                          interval = 500,
                          trim_regex = "v_unstr|u_str|str_unscaled|tau_") {
    ## Just trims out the columns we don't care about from a stan_csv
    ## This ends up saving about 75% of the memory.
    ## NOTE: parameters we don't care about: u_str, v_unstr, str_unscaled.
    ##       To remove other columns, just add their regex to trim_regex
    ##
    ## This function is **very** slow, but necessary to save memory. Python
    ## version is much much faster, but calling two langauges seems excessive.
    ## Should only need to do this once per chain.
    con_in  <- file(input_file, open = "r")
    con_out <- file(output_file, open = "w")
    counter <- 0
    
    while (length(l <-
                  readLines(con_in, n = 1, warn = FALSE)) > 0) {
        ## Deal with comments first
        if (substr(l, 1, 1) == "#") {
            if (substr(l, 3, 6) == "thin") {
                if (thin  > 1) {
                    l1 <- strsplit(l, "=")[[1]][1]
                    l <- paste0(l1, "=", thin)
                }
            }
            cat(l, file = con_out, sep = "\n")
        } else if (substr(l, 1, 4) == "lp__") {
            ## If you hit the header, then find the indices of columns
            ## we want to keep
            ll <- unlist(strsplit(l, ","))
            ix_to_keep <- !grepl(trim_regex, ll)
            
            ## Then select what we want to keep and write new line to file
            l <- paste0(ll[ix_to_keep], collapse = ",")
            cat(l, file = con_out, sep = "\n")
            
            if (verbose) {
                cat(
                    sprintf(
                        "Original Columns: %i \nKept Columns: %i \nDropped Columns: %i\n",
                        length(ll),
                        sum(ix_to_keep),
                        length(ll) - sum(ix_to_keep)
                    )
                )
            }
        } else {
            ## Update counter
            counter <- counter + 1
            
            ## Thin if necessary
            if (counter %% thin == 0) {
                ll <- unlist(strsplit(l, ","))
                ## Then select what we want to keep and write new line to file
                l <- paste0(ll[ix_to_keep], collapse = ",")
                cat(l, file = con_out, sep = "\n")
            }
        }
        
        if (verbose && counter > 0 && (counter %% interval) == 0) {
            cat(sprintf("Data rows written: %i\n", counter))
        }
    }
    close(con_in)
    close(con_out)
}
