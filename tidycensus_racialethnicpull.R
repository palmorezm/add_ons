# Packages
require(tidycensus)
require(dplyr)

#! User will need to specify their own API key

# function ideas

# function 1 - pulls regular "total population" data for the state of WI across year 
# user selects years (as a column/field)
# user selects states (as a column/field)
# it automatically fills acs5 whenever possible
# it automatically fills most common inputs of tidycensus acs function
# outputs rdata 
county_population <- function(state, end_year, start_year){
  tmp_df <- data.frame()
    for (i in 0:(end_year-start_year)){ 
      year = (end_year[[1]] - i)
      print(paste("Processing Data for", year[[1]]))
      tmp <- tidycensus::get_acs(
        variables = c("B01003_001"),
        geography = "county",
        state = state[[1]],
        moe = 95,
        zcta = NULL,
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy",
        survey = "acs5",
        year = year[[1]],
      )
      tmp_df <- rbind(tmp_df, tmp)
      }
  return(tmp_df %>% 
    mutate(year = year[[1]])
    )
}

# funtion 2 - pulls a slew of race/ethnic data
# names appropriatly based on most common categories from ACS
county_race_ethnicity_population <- function(state, end_year, start_year){
  tmp_df <- data.frame()
    for (i in 0:(end_year-start_year)){ 
      year = (end_year[[1]] - i)
      print(paste("Processing Data for", year[[1]]))
      tmp <- tidycensus::get_acs(
        variables = c(
          # White Alone
          "B01001A_001", 
          # Black or African American Alone
          "B01001B_001", 
          # American Indian and Alaska Native Alone
          "B01001C_001",
          # Asian Alone 
          "B01001D_001", 
          # Native Hawaiian and Other Pacific Islander
          "B01001E_001", 
          # Some Other Race alone
          "B01001F_001", 
          # Two or More Races
          "B01001G_001", 
          # White Alone Not Hispanic or Latino
          "B01001H_001", 
          # Hispanic or Latino
          "B01001I_001"
        ),
        geography = "county",
        state = state[[1]],
        moe = 95,
        zcta = NULL,
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy",
        survey = "acs5",
        year = year[[1]],
      )
      tmp_df <- rbind(tmp_df, tmp)
      }
  return(tmp_df %>% 
           mutate(
            year = year[[1]],
            race = case_when(
              variable == "B01001A_001" ~ "White Alone",
              variable == "B01001B_001" ~ "Black or African American Alone",
              variable == "B01001C_001" ~ "American Indian and Alaska Native Alone",
              variable == "B01001D_001" ~ "Asian Alone",
              variable == "B01001E_001" ~ "Native Hawaiian and Other Pacific Islander Alone",
              variable == "B01001F_001" ~ "Some Other Race Alone",
              variable == "B01001G_001" ~ "Two or More Races",
              variable == "B01001H_001" ~ "White Alone Not Hispanic or Latino",
              variable == "B01001I_001" ~ "Hispanic or Latino",
              )
           )
         )
}


# B19013_001
#     MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS 
#     (IN 2020 INFLATION-ADJUSTED DOLLARS)
#         B19013A_001 (White Alone) and other races available

county_race_ethnicity_median_household_income <- function(state, end_year, start_year){
  tmp_df <- data.frame()
    for (i in 0:(end_year-start_year)){ 
      year = (end_year[[1]] - i)
      print(paste("Processing Data for", year[[1]]))
      tmp <- tidycensus::get_acs(
        variables = c(
          # White Alone
          "B19013A_001", 
          # Black or African American Alone
          "B19013B_001", 
          # American Indian and Alaska Native Alone
          "B19013C_001",
          # Asian Alone 
          "B19013D_001", 
          # Native Hawaiian and Other Pacific Islander
          "B19013E_001", 
          # Some Other Race alone
          "B19013F_001", 
          # Two or More Races
          "B19013G_001", 
          # White Alone Not Hispanic or Latino
          "B19013H_001", 
          # Hispanic or Latino
          "B19013I_001"
        ),
        geography = "county",
        state = state[[1]],
        moe = 95,
        zcta = NULL,
        show_call = FALSE,
        cache_table = TRUE,
        output = "tidy",
        survey = "acs5",
        year = year[[1]],
      )
      tmp_df <- rbind(tmp_df, tmp)
      }
  return(tmp_df %>% 
           mutate(
            year = year[[1]],
            race = case_when(
              variable == "B19013A_001" ~ "White Alone",
              variable == "B19013B_001" ~ "Black or African American Alone",
              variable == "B19013C_001" ~ "American Indian and Alaska Native Alone",
              variable == "B19013D_001" ~ "Asian Alone",
              variable == "B19013E_001" ~ "Native Hawaiian and Other Pacific Islander Alone",
              variable == "B19013F_001" ~ "Some Other Race Alone",
              variable == "B19013G_001" ~ "Two or More Races",
              variable == "B19013H_001" ~ "White Alone Not Hispanic or Latino",
              variable == "B19013I_001" ~ "Hispanic or Latino",
              )
           )
         )
}

# Tables of interest:
# B19025_001
#     AGGREGATE HOUSEHOLD INCOME IN THE PAST 12 MONTHS 
#     (IN 2020 INFLATION-ADJUSTED DOLLARS)
#         B19025_001 (White Alone) and other races available
# B19113_001
#     MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS 
#     (IN 2020 INFLATION-ADJUSTED DOLLARS)
#         B19113A_001 (White Alone) and other races available
# B19202_001
#     MEDIAN NONFAMILY HOUSEHOLD INCOME IN THE PAST 12 MONTHS
#     (IN 2020 INFLATION-ADJUSTED DOLLARS) 
#         B19202A_001 (White Alone) and other races available
# B07412_001 
#     Geographical Mobility in the Past Year by Poverty Status 
#     in the Past 12 Months for Residence 1 Year Ago in the 
#     United States
# county_health_coverage_population?


# function 3 - exports all data as spreadsheet(s)
# write rdata from any function to your current working directory
# or user-specified file path location
files <- mget(ls())
for (i in 1:length(files)){
  write.csv(files[[i]], paste(names(files[i]), ".csv", sep = ""))
}



# What if the user was able to select from a list of data we have?
# Since table names are sometimes confusing and certainly 
# unknonwn to most people in gov, we could name it after
# what we call it colloquially, then host it 
# saving people a lot of time and making it harder to make a mistake

