## Read
if(!"county_raw" %in% ls()) source("./county_raw.R")
county_clean <- county_raw
rm("county_raw")

## Filter counties with cases
county_clean %<>% 
  filter(!is.na(county))

## Clean popdensity
popdensity_ = county_clean %>%
  distinct(fips, popdensity) %>%
  pull(popdensity)
#summary(popdensity_)
# county_clean %>%
#   filter(is.na(popdensity)) %>% view

# ----removed for hurricane project
# county_clean %<>% 
#   filter(!is.na(popdensity), #removes na
#          popdensity > quantile(popdensity_, 0.1, na.rm = TRUE)
#   )

## Create daily case and death count columns
county_clean %<>%  
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(cases = diff(c(0,cum_cases)), 
         deaths = diff(c(0,cum_deaths))) %>% 
  ungroup()

## Create per capita and per sq deaths and cases
county_clean <- county_clean  %>% 
  arrange(fips, date) %>%
  mutate(cum_deaths_per_cap = cum_deaths / pop,
         cum_deaths_per_sq_mi = cum_deaths / sq_mi, 
         cum_cases_per_cap = cum_cases / pop,
         cum_cases_per_sq_mi = cum_cases / sq_mi)

## Create index columns
county_clean %<>% 
  group_by(fips) %>% 
  arrange(date) %>% 
  mutate(index = row_number(), 
         index_desc = sort(index, decreasing = TRUE)) %>% 
  ungroup()

county_clean <- arrange(county_clean, fips, index)

rm(list=c("popdensity_"))
