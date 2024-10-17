# Packages
require(tidycensus)

#! User will need to specify their own API key

# function ideas

# function 1 - pulls regular "total population" data for the state of WI across year 
# user selects years (as a column/field)
# user selects states (as a column/field)
# it automatically fills acs5 whenever possible
# it automatically fills most common inputs of tidycensus acs function
# outputs rdata 
county_population <- function(state, year) {
    tmp_year_selected <- tidycensus::get_acs(
        variables = c("B01003_001"), 
        geography = "county", 
        state = state[[1]], 
        moe = 95, 
        zcta = NULL, 
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy", 
        survey = "acs5"
        year = year[[1]], 
    )
    # Seems Tidycensus/Tyler Tech/ Census Bureau currently only allows 1 year?
    # Rather than have the user run the same function 5 times for each request
    # then combining, use it once, and remove what you dont need/reduce as needed
    # based on the data - more r users in the state have the ability to 
    # filter data and reduce it's size than to add or create or simulate
    # most (~65% of users) also know dplyr's filter() for year and subset()
     tmp_year_minus1 <- tidycensus::get_acs(
        variables = c("B01003_001"), 
        geography = "county", 
        state = state[[1]], 
        moe = 95, 
        zcta = NULL, 
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy", 
        survey = "acs5"
        year = year[[1]]-1, 
    )
     tmp_year_minus2 <- tidycensus::get_acs(
        variables = c("B01003_001"), 
        geography = "county", 
        state = state[[1]], 
        moe = 95, 
        zcta = NULL, 
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy", 
        survey = "acs5"
        year = year[[1]]-2, 
    )
     tmp_year_minus3 <- tidycensus::get_acs(
        variables = c("B01003_001"), 
        geography = "county", 
        state = state[[1]], 
        moe = 95, 
        zcta = NULL, 
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy", 
        survey = "acs5"
        year = year[[1]]-3, 
    )
     tmp_year_minus4 <- tidycensus::get_acs(
        variables = c("B01003_001"), 
        geography = "county", 
        state = state[[1]], 
        moe = 95, 
        zcta = NULL, 
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy", 
        survey = "acs5"
        year = year[[1]]-4, 
    )
    tmp_5yr <- rbind(tmp_year_selected, tmp_year_minus1, tmp_year_minus2, tmp_year_minus3, tmp_year_minus4)
}

# Or alternatively: 
county_population <- function(state, end_year, start_year) {
    county_population <- data.frame()
    # let have an end_year and begining year for the user 
    for (i in 0:start_year) { 
        tmp_end_year_selected <- tidycensus::get_acs(
        variables = c("B01003_001"), 
        geography = "county", 
        state = state[[1]], 
        moe = 95, 
        zcta = NULL, 
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy", 
        survey = "acs5"
        end_year = end_year[[1]] - i, 
        ) %>% 
        mutate(end_year = end_year[[1]] - i)
        df <- rbind(df, tmp)
    }
}
# funtion 2 - pulls a slew of race/ethnic data
# names appropriatly based on most common categories from ACS

# Global Factors
key <- NULL # hidden but shows API key 
# for security, this key should be loaded in each session
year <- 2022 # input year of data 
acsx <- "acs5" # Select version and year of acs data
output <- "tidy" # format of data output
state <- 55 # FIPS code location of interest
zcta <- NULL # tabulate by zip code tabulation areas
moe <- 90 # acceptable numeric margin of error 
geography <- "county" # location specific data
show_call <- FALSE # if TRUE, display call made to Census API
cache_table <- TRUE # whether or not to cache table names for faster future access. Defaults to FALSE

# Table Pattern:
# B01001A_001 = White Alone
# B01001B_001 = Black or African American Alone
# B01001C_001 = American Indian and Alaskan Native Alone
# B01001D_001 = Asian Alone 
# Tidycensus defaults to B01001D_001E (Notice 'E' is esitmate) - this changes if Margin of Error values are present for certain variables

race_ethnicity <- function(state, year){
# this one is not looped as easy - each race/ethnicity has it's own letter

    pop_blackorafricanamerican_alone <- data.frame()
        for (i in 0:10) {
        tmp <- tidycensus::get_acs(
            geography = geography[[1]],
            variables = c("B01001B_001"),
            table = NULL,
            cache_table = cache_table[[1]],
            year = year[[1]],
            output = output[1],
            state = state[[1]],
            county = NULL,
            zcta = zcta[1],
            geometry = FALSE,
            keep_geo_vars = FALSE,
            shift_geo = FALSE,
            summary_var = NULL,
            key = key[1],
            moe_level = moe[[1]],
            survey = acsx[[1]],
            show_call = show_call[[1]]
            ) %>% 
        mutate(
            year = year - i, 
            race = "Black or African American Alone")
        pop_blackorafricanamerican_alone <- rbind(df, tmp)
        }
    pop_asian_alone <- data.frame()
        for (i in 0:10) {
        tmp <- tidycensus::get_acs(
            geography = geography[[1]],
            variables = c("B01001D_001"),
            table = NULL,
            cache_table = cache_table[[1]],
            year = year[[1]],
            output = output[1],
            state = state[[1]],
            county = NULL,
            zcta = zcta[1],
            geometry = FALSE,
            keep_geo_vars = FALSE,
            shift_geo = FALSE,
            summary_var = NULL,
            key = key[1],
            moe_level = moe[[1]],
            survey = acsx[[1]],
            show_call = show_call[[1]]
            ) %>% 
        mutate(
            year = year - i, 
            race = "Asian Alone")
        pop_asian_alone <- rbind(df, tmp)
        }
}


# function 3 - exports all data as spreadsheet(s)
# write rdata from any function to your current working directory
# or user-specified file path location

