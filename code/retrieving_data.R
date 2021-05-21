###################################
### README ########################
###################################

# The following code retrieves:

## (1) 2020 Presidential elections results by county
## (2) Employed/unemployed people by county by month for 2019-2020 years

rm(list=ls())

# Loading Libraries
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(blsAPI)
library(jsonlite)

# Setting Up WD
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Spring 2021/Data Visualization/Week 8/HW8"
setwd(wd)

# 1 -- 2020 Presidential Elections Results by county
#   -- Data developed using The Guardian, townhall.com, Fox News, Politico, and the New York Times
#   -- More details: https://github.com/tonmcg/US_County_Level_Election_Results_08-20

if (!file.exists(file.path("data", "elections_2020.csv")))
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
                destfile = "elections_2020.csv", mode = "wb")

elections_2020 <- read_csv(file.path("data", "elections_2020.csv"))

# 2 -- Employment/Unemployment data by county
#   -- Data retrieved from the BLS using BLSapi
#   -- More details: https://cran.r-project.org/web/packages/blsAPI/blsAPI.pdf

if (!file.exists(file.path("data", "laus_data.csv"))) {
  bls_key  <- "e54e539aed6045f29dbc51133d1ad7fe"
  
  counties <- sort(unique(results_2020$GEOID))
  seriesid <- c(paste("LAUCN", counties, "000000000", 4, sep = ""),   # Employed
                paste("LAUCN", counties, "000000000", 5, sep = ""))   # Unemployed
  
  list_laus <- list()
  
  for (i in 1:( length(seriesid) / 50 + 1)) {
    
    start <- (i - 1) * 50 + 1
    end   <- start + 49
    
    if (end >= length(seriesid))
      end <- length(seriesid)
    
    payload  <- list("seriesid"  = seriesid[start:end],
                     "startyear" = 2019,
                     "endyear"   = 2020,
                     "registrationKey" = bls_key)
    
    list_laus[[i]] <- blsAPI(payload, api_version = 2, return_data_frame = TRUE)
    
    if (i %% 50 == 0)       # After 50 requests proceeds
      Sys.sleep(15)         # A waiting time due to API rule
    
    cat("Retrieving:", paste0(end / length(seriesid) * 100, "%"), "\n")
    
  }
  
  laus_data <- do.call("rbind", list_laus)
  
  write_csv(laus_data, "laus_data.csv")
}


# Calculating the Jan-Oct 2020 and Jan-Oct 2019 unemployment rates

laus_data <-
  read_csv(file.path("data", "laus_data.csv")) %>%
  mutate(seriesID = parse_number(seriesID),                       # seriesID contains: fips code & status of employment
         GEOID = seriesID %/% 10000000000,                              
         GEOID = str_pad(GEOID, width = 5, side = "left", pad = 0),
         type  = seriesID %% 10000000000,
         type = case_when(type == 4 ~ "unemployed",
                          type == 5 ~ "employed")) %>%
  filter(!period == c(11, 12)) %>%                          # Removing November and December months from both years
  group_by(GEOID, year, type) %>%
  summarise(jobs = mean(value, na.rm = TRUE)) %>%           # Average by GEOID/year/type
  mutate(share = jobs / sum(jobs, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(type == "unemployed") %>%
  pivot_wider(id_cols = GEOID, names_from = year, values_from = share, names_prefix = "unemployment_") %>%
  mutate(delta_unemployment = unemployment_2020 - unemployment_2019)
 
unemployment_elections <-
  laus_data %>%
  left_join(elections_2020, by = c("GEOID" = "county_fips")) 

write_csv(unemployment_elections, file.path("data", "unemployment_elections.csv"))
