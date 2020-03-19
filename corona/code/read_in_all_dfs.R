
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in data and clean
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

raw_data <- util_scrape_table("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

clean_data <- raw_data %>% row_to_names(1) %>% clean_names %>% remove_empty() %>% 
  gather(date, infections, -1:-4) %>% 
  mutate(date = str_remove_all(date, "x")) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(infections = parse_number(infections))


# clean_data %>% filter(country_region %in% countries, 
#                       infections != 0) %>% 
#   group_by(date, country_region) %>% 
#   summarise(infections = sum(infections)) %>% 
#   ungroup %>% 
#   spread(country_region, infections) %>% 
#   xlopen


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in country and population data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
countries_df <- read_rds("table/population_df.rds")
