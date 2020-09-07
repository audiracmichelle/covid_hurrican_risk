library(tidyverse)
library(magrittr)
library(feather)

## Read county_counts and county_desc
county_counts <- read_feather("./county_counts.feather")
county_desc <- read_feather("./county_desc.feather")

## Join county_counts with county_desc
#length(unique(county_counts$fips))
#length(unique(county_desc$fips))

#idx <- !county_desc$fips %in% county_counts$fips
#county_desc[idx, c("state_code", "county_name", "fips")] %>%  view()

#dim(county_counts)
#dim(county_desc)
county_raw <- county_counts %>% 
  filter(fips != "36xxx") %>%
  full_join(county_desc, by = "fips")
#dim(county_raw)

## New York City counts
diff_fips <- setdiff(county_desc$fips, county_counts$fips)

ny_data <- county_desc %>% 
  filter(fips %in% diff_fips, state_code == "NY")

ny_counts <- county_counts %>% 
  filter(fips == "36xxx")

for(r in 1:nrow(ny_data)) {
  xx_counts <- ny_counts
  xx_counts$county <- ny_data$county_name[r]
  xx_counts$fips <- ny_data$fips[r]
  xx_counts %<>% 
    mutate(cum_cases = 
             round(cum_cases * ny_data$popdensity[r] / sum(ny_data$popdensity)), 
           cum_deaths = 
             round(cum_deaths * ny_data$popdensity[r] / sum(ny_data$popdensity))) %>% 
    left_join(ny_data)
  county_raw <- rbind(county_raw, xx_counts)
}
#dim(county_raw)

# county_raw %>%
#   filter(state == "New York",
#          grepl("county", county)) %>% view

#county_clean$county[which(county_clean$cum_cases< 1)]

rm(list=c("county_counts", 
             "county_desc", 
             "diff_fips", 
             "ny_counts", 
             "ny_data", 
             "r", 
             "xx_counts"))
