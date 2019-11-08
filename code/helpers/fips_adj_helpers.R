## Imports ----
library(tidyverse)


## Adding dummy FIPS ----
add_dummy_county_ids <- function(county_adj) {
    ## Just returns a County Adjacency matrix with two new columns -- each
    ## representing a dummy variable from 1:n_counties
    
    ## Sort by FIPS
    new_adj <- dplyr::arrange(county_adj, fips_i, fips_j)
    
    ## Make a dummy dictionary that with FIPS as the key and a dummy as value.
    dummies <- as.list(1:length(unique(new_adj$fips_i)))
    names(dummies) <- unique(new_adj$fips_i)
    
    ## Then make a dummy for node_i and node_j
    new_adj$dummy_i <-
        unlist(dummies[new_adj$fips_i], use.names = FALSE)
    new_adj$dummy_j <-
        unlist(dummies[new_adj$fips_j], use.names = FALSE)
    
    return(new_adj)
}


add_dummy_state_ids <- function(county_adj) {
    ## Just returns a County Adjacency matrix with two new columns -- each
    ## representing a dummy variable from 1:n_states
    
    ## Sort by FIPS
    new_adj <- dplyr::arrange(county_adj, fips_i, fips_j)
    
    ## Make a dummy dictionary that with FIPS as the key and a dummy as value.
    dummies <- as.list(1:length(unique(new_adj$state_i)))
    names(dummies) <- unique(new_adj$state_i)
    
    ## Then make a dummy for node_i and node_j
    new_adj$dummy_state_i <-
        unlist(dummies[new_adj$state_i], use.names = FALSE)
    new_adj$dummy_state_j <-
        unlist(dummies[new_adj$state_j], use.names = FALSE)
    
    return(new_adj)
}


## Fixing bedford city ----
fix_bedford_city_fips <- function(fips_column) {
    ## According to CMF documentation, in 2011, Bedford City, VA got lumped
    ## in with Bedford county. Change FIPS to accommodate.
    
    x <- ifelse(fips_column == "51515", "51019", fips_column)
    return(x)
}


## Adjacency matrix munging ----
make_winbugs_adj <- function(county_adj) {
    ## Takes a US Census County Adjacency dataframe and returns a neighbor list
    ## in WinBUGS format. Specifically:
    ##  (1). A vector of length n_nodes each element representing
    ##       number of neighbors
    ##  (2)  A vector of length neighbor pairs corresponding to (1).
    ##
    ##  Thus, if the first element of the first vector is 7, the first 7
    ##  elements of the second vector represent the neighbors of node 1.
    
    neighbors_i <- county_adj %>%
        dplyr::group_by(dummy_i) %>%
        dplyr::summarize(neighbors_i = dplyr::n()) %>%
        dplyr::pull(neighbors_i)
    
    neigh_list <- county_adj$dummy_j
    
    return(list(num_neigh = neighbors_i,
                neigh_list = neigh_list))
}


make_inla_adj <- function(county_adj, out_file = FALSE) {
    ## INLA requires adj matrices in the form of:
    ## > number_of_nodes
    ## > node1 number_of_neighbors neighbor_1 neighbor_2
    ## Such that you end up with (number of nodes + 1) rows with the first row
    ## indicating the total number of nodes and subsequent rows indicating the
    ## node number, number of neighbors, and specifying neighbors.
    
    ## This function converts a County Adjacency dataframe into an neighbor list
    ## of the INLA specification.
    
    num <- county_adj %>%
        dplyr::group_by(dummy_i) %>%
        dplyr::summarize(neighbors_i = dplyr::n()) %>%
        dplyr::pull(neighbors_i)
    
    adj <- county_adj$dummy_j
    
    if (sum(num) != length(adj)) {
        return(print("Number of nodes and length of adj do not match."))
    }
    
    adj_part2 <- vector("list", length(num))
    
    # For each row, use the format:
    #   node_number num_neighbors neigh1 neigh2 neigh3
    start_ix <- 1
    end_ix <- 0
    for (i in 1:length(num)) {
        n_neigh <- num[i]
        end_ix <- end_ix + n_neigh
        adj_part2[[i]] <- c(i, n_neigh, adj[start_ix:end_ix])
        start_ix <- start_ix + n_neigh
    }
    
    # First row is total number of nodes
    adj_part2 <- append(length(num), adj_part2)
    
    if (out_file != FALSE) {
        # Now save file
        writeLines(unlist(lapply(adj_part2, paste, collapse = " ")),
                   con = out_file)
    } else {
        return(adj_part2)
    }
}


make_stan_adj <- function(county_adj) {
    ## Converts a County Adjacency dataframe into a traditional adjacency matrix
    ## of size n_counties x n_counties with 1 representing neighbors.
    
    ## Number of neighbors
    num <- county_adj %>%
        dplyr::group_by(dummy_i) %>%
        dplyr::summarize(neighbors_i = dplyr::n()) %>%
        dplyr::pull(neighbors_i)
    
    ## List of neighbors
    adj <- county_adj$dummy_j
    
    ## Create an appropriately sized matrix
    n_nodes <- length(num)
    A <- matrix(0L, nrow = n_nodes, ncol = n_nodes)
    
    ## Loop through num and adj and assign 1 to neighbors
    start_ix <- end_ix <- 0
    for (node in seq_along(num)) {
        for (n_neigh in num[node]) {
            start_ix  <- end_ix + 1
            end_ix    <- end_ix + n_neigh
            
            neighbors <- adj[start_ix:end_ix]
            
            A[node, neighbors] <- 1L
        }
    }
    return(A)
}


return_sparse_parts <- function(adj_matrix) {
    ## This is taken from mbjoseph CARstan repo -- returns a sparse
    ## representation of an adjacency matrix in the format that Stan needs
    ## Number of neighbors
    D_sparse <- rowSums(adj_matrix)
    
    ## Kyle Foreman script -- undirected graph so second line removes dupes
    W_sparse <- which(adj_matrix == 1, arr.ind = TRUE)
    W_sparse <- W_sparse[W_sparse[, 1] < W_sparse[, 2], ]
    
    ## Get eigenvalues of D^(-.5) * W * D^(-.5) for determinant computations
    invsqrtD <- diag(1 / sqrt(D_sparse))
    quadformDAD <- invsqrtD %*% adj_matrix %*% invsqrtD
    lambdas <- eigen(quadformDAD)$values
    
    ## Number of edges
    W_n <- nrow(W_sparse)
    
    return(list(
        D_sparse = D_sparse,
        W_sparse = W_sparse,
        lambdas = lambdas,
        W_n = W_n
    ))
}
