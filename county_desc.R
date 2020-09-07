library(tidyverse)
library(magrittr)
library(feather)
library(data.table)

##   Get intervention and county description data from JHU

### -- Intervention Data form JHU Github  --- ###

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
interventions_raw <- fread("https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/interventions.csv")

# Rename variables
setnames(interventions_raw, 
         old = c("FIPS","STATE","AREA_NAME",
                 "stay at home", ">50 gatherings", ">500 gatherings", "public schools", "restaurant dine-in", "entertainment/gym", "federal guidelines", "foreign travel ban"),
         new = c("fips","state_code","county_name", 
                 "stayhome", "gt50", "gt500", "schools", "restaurants", "entertainment", "federal", "travel"))

# Format Dates
interventions_raw[, stayhome := as.Date(stayhome, origin = "0001/01/01")]
interventions_raw[, gt50 := as.Date(gt50, origin = "0001/01/01")]
interventions_raw[, gt500 := as.Date(gt500, origin = "0001/01/01")]
interventions_raw[, schools := as.Date(schools, origin = "0001/01/01")]
interventions_raw[, restaurants := as.Date(restaurants, origin = "0001/01/01")]
interventions_raw[, entertainment := as.Date(entertainment, origin = "0001/01/01")]
interventions_raw[, federal := as.Date(federal, origin = "0001/01/01")]
interventions_raw[, travel := as.Date(travel, origin = "0001/01/01")]

# Format FIPS to have 5 characters
interventions_raw[, fips := formatC(fips, width = 5, flag = "0")] # - make sure fips is 5 digits with leading 0

## ---------------------------------- ###


### -- County Description Data form JHU Github  --- ###

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
jhudat <- fread("https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/counties.csv")

# Format FIPS to have 5 characters
jhudat[, FIPS := formatC(FIPS, width = 5, flag = "0")] # - make sure fips is 5 digits with leading 0

# - FOR NOW: just grab the population and population density variables, update later to include more
county_vars = c("POP_ESTIMATE_2018", 
                "Density per square mile of land area - Population",
                "Density per square mile of land area - Housing units", 
                "Area in square miles - Land area", 
                "Rural-urban_Continuum Code_2013",                                                                                                                
                "Urban_Influence_Code_2013", 
                "Percent of adults completing some college or associate's degree 2014-18",                                                                          
                "Percent of adults with a bachelor's degree or higher 2014-18", 
                "Median_Household_Income_2018",
                "PCTPOVALL_2018") 

jhudat = jhudat[, c("FIPS", county_vars), with = FALSE]

setnames(jhudat, 
         old = c("FIPS", county_vars), 
         new = c("fips", "pop", "popdensity",
                 "housedensity", "sq_mi", 
                 "rural_urban", "urban_influence", 
                 "college", "degree", "household_income", "percent_poverty"))

# -----------------------------------------------------------

### -- Merge 1: JHU interventions with JHU County Descrition  --- ###

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --#
# - Done here because of common formatting
# - NOTE: if we add more JHU data, do so here for similar reason

dim(jhudat)
dim(interventions_raw)
jhudat = merge(jhudat, interventions_raw, by = "fips")
dim(jhudat)

# - Separate out county dat for counties and for states
jhudat[, "is_county":= !(substr(fips, 3,5) == "000")]
jhudat[state_code=="DC", ]$is_county = TRUE # - Manually include DC as a "county"

jhudat_county = jhudat[is_county==TRUE, ] # 3221 counties, including Washington DC
jhudat_state = jhudat[is_county==FALSE, ] # 52 states, includes puerto rico and record for "United States"

# - Give each county it's respective state-level variable
county_desc = merge(jhudat_county, jhudat_state, by = "state_code", suffixes = c("", "_state"), all.x = TRUE)

county_desc %<>% 
  select(-county_name_state, -is_county, -is_county_state, -rural_urban_state, -urban_influence_state)

write_feather(county_desc, "./county_desc.feather")
