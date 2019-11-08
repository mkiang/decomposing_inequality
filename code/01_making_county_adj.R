## Imports ----
library(tidyverse)
source('./code/helpers/us_constants.R')
source('./code/helpers/download_helpers.R')
source('./code/helpers/fips_adj_helpers.R')
source('./code/helpers/misc_helpers.R')

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
raw_folder <- cfig$raw_dir
sav_folder <- cfig$data_dir
save_full_adj <- cfig$save_full_adj

## Make folders ----
mkdir_p(sav_folder)
mkdir_p(raw_folder)

## Import data ----
county_adj_file_loc <-
    sprintf("%s/county_adjacency.txt", raw_folder)

if (!file.exists(county_adj_file_loc)) {
    download_raw_census_adj(county_adj_file_loc)
}

county_adj_raw <- readr::read_delim(
    county_adj_file_loc,
    delim = "\t",
    escape_double = FALSE,
    col_names = c("name_i", "fips_i",
                  "name_j", "fips_j"),
    trim_ws = TRUE
)

## Forward fill ----
county_adj <- zoo::na.locf(county_adj_raw)

## Remove self loops and add state fips ----
county_adj <- county_adj %>%
    dplyr::filter(fips_i != fips_j) %>%
    dplyr::mutate(state_i = substr(fips_i, 1, 2),
                  state_j = substr(fips_j, 1, 2))

## Fix FIPS ----
## Bedford City got lumped together with Bedford county in CMF data
## See documentation for more.
# county_adj <- county_adj %>%
#     mutate(fips_i = fix_bedford_city_fips(fips_i),
#            fips_j = fix_bedford_city_fips(fips_j))
county_adj <- county_adj %>%
    dplyr::filter(fips_i != "51515",
                  fips_j != "51515")

## Save if specified ----
if (save_full_adj) {
    saveRDS(county_adj,
            sprintf("%s/county_adj_cleaned_full.RDS", raw_folder))
}

## Now remove states/territories we won't use ----
sub_adj <- dplyr::filter(county_adj,
                         state_i %in% us$allb,
                         state_j %in% us$allb)

sub_adj <- add_dummy_county_ids(sub_adj)
sub_adj <- add_dummy_state_ids(sub_adj)

## Save this mapping of adjacency and state/county dummy codes ----
readr::write_csv(sub_adj, path = sprintf("%s/county_adj_with_dummy.csv", sav_folder))

## Save a WinBUGS representation of adjacency ----
winbugs_adj <- make_winbugs_adj(sub_adj)
saveRDS(winbugs_adj, file = sprintf("%s/county_adj_winbugs.RDS", sav_folder))

## Save a true adjacency matrix ----
adj_mat <- make_stan_adj(sub_adj)
saveRDS(adj_mat, file = sprintf("%s/county_adj_matrix.RDS", sav_folder))

## Save a sparse adjacency representation (for Stan) ----
sparse_A <- return_sparse_parts(adj_mat)
saveRDS(sparse_A,
        file = sprintf("%s/county_adj_sparse_parts.RDS", sav_folder))

## Save a weighted neighbor list for spatial calculations ----
adj_listw <- spdep::mat2listw(adj_mat, style = "W")
saveRDS(adj_listw, file = sprintf('%s/county_adj_w_list.RDS', sav_folder))

## Save a unique mapping of FIPS to state and county dummy codes ----
fips_to_dummy_mapping <- sub_adj %>%
    dplyr::select(
        county_name = name_i,
        fipschar = fips_i,
        fips_st = state_i,
        c_idx = dummy_i,
        s_idx = dummy_state_i
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(st_abbr = unlist(lapply(strsplit(county_name, split = ", "),
                                          function(x)
                                              x[[2]])))
saveRDS(fips_to_dummy_mapping,
        sprintf("%s/fips_to_dummy_mappings.RDS", sav_folder))
