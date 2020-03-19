# This analysis is adapted from 
# https://blog.ephorie.de/covid-19-the-case-of-germany
# Data Source: https://github.com/CSSEGISandData/COVID-19


source("code/functions.R")
source("code/read_in_all_dfs.R")


#Generate the forecast
x_list <- map(countries_df$country, corona_fcast)
names(x_list) <- countries_df$country


# Get key stats
map_df(1:length(x_list), ~get_fcast_stats(x_list[[.x]])) %>% 
  mutate(pct_infected = round(peak_no / population, 4)) %>% 
  mutate_at(c(3, 4), .funs = comma) %>%  
  select(-Ro, dplyr::everything(), Ro) %>% 
  arrange(pct_infected) %>% 
  mutate_at(5, .funs = ~.*100)

# Save output
# saveRDS(x_list, "output/fcast_20Mar20.rds")

# Questions
# 1. Why do some countries have a high Ro and a corresponding pct infection rate
# 2. Problem countries - South Korea, Japan, Iran

# Is the problem in forecast related to data quality or do we need to tune model parameters?


# Generate plots
walk(1:length(x_list), ~corona_plots(x_list[[.x]]))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Individual plots manually
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

corona_plots(corona_fcast("Italy", points_to_plot_in_short_plot = 30))
corona_plots(corona_fcast("France", points_to_plot_in_short_plot = 30))
corona_plots(corona_fcast("Germany", points_to_plot_in_short_plot = 30))

corona_plots(x_list[["Spain"]])


corona_plots(corona_fcast("India"))
corona_plots(corona_fcast("Pakistan"))
corona_plots(corona_fcast("China"))

x_list[["China"]][["forecast_data"]] %>%
  mutate_at(c(2, 3), .funs = comma) %>% 
  knitr::kable()
