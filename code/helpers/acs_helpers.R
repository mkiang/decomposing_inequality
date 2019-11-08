library(acs)

## ACS helper functions
get_table_name <- function(acs_object) {
    just_title <- select_(acs_object@results,
                          `Table Number` = "table.number",
                          `Table Name` = "table.name")[1,]
    return(just_title)
}

print_table_vars <- function(acs_object) {
    just_vars <- select_(acs_object@results,
                         `Variable Code` = "variable.code",
                         `Variable Name` = "variable.name")
    return(just_vars)
}

make_fips <- function(acs_obj) {
    ## Takes a county-level acs object and returns five digit FIPS for rows
    fips <- apply(
        geography(acs_obj),
        1,
        FUN = function(x)
            paste0(sprintf(as.numeric(x['state']),
                           fmt = '%02d'), x['county'])
    )
    return(fips)
}

acs_to_df <- function(table_name, geo, year = 2015) {
    acs_obj <- acs.fetch(year, geography = geo,
                         table.number = table_name)
    acs_df <- as.data.frame(estimate(acs_obj))
    names(acs_df) <- tolower(names(acs_df))
    acs_df$fipschar <- make_fips(acs_obj)
    return(acs_df)
}
