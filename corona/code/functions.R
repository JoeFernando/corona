#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Load libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(data.table)
library(openxlsx)
library(janitor)
library(lubridate)
library(tidyverse)
library(broom)
library(deSolve)
library(platus)
library(scales)
library(patchwork)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# function to scrape data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

util_scrape_table <- function(link, table_no = 1){
  
  require(xml2)
  require(rvest)
  require(janitor)
  require(tidyverse)
  
  raw_webpage <- read_html(link)
  
  html_table(raw_webpage, fill = TRUE)[[table_no]]
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Output functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


corona_plots <- function(x){
  
  x11(title = x[[1]])
  plot(x[[2]] + x[[3]] + plot_layout(ncol = 1))
  
  
}


get_fcast_stats <- function(stats_list){
  
  data.frame(
    country   = stats_list %>% purrr::pluck("country_name"),
    peak_date = stats_list %>% purrr::pluck("height_of_pandemic_date"),
    peak_no   = stats_list %>% purrr::pluck("height_of_pandemic"),
    population = stats_list %>% purrr::pluck("Population"),
    Ro        = stats_list %>% purrr::pluck("Ro")
  )
  
}  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# forecast function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


corona_fcast <- function(country_to_model  = countries_df$country[1],
                         days_to_model     = 1:80,
                         points_to_plot_in_short_plot = 15,
                         
                         actual_data = clean_data,
                         details_df  = countries_df){
  
  
  # Get variables to generate forecast and plots
  filter_from_date   = details_df %>% filter(country == country_to_model) %>% pull(filter_date)
  country_population = details_df %>% filter(country == country_to_model) %>% pull(population)
  
  # Get the data you need to model
  df_to_model <- actual_data %>% 
    
    filter(country_region == country_to_model,
           date >= filter_from_date) %>% 
    group_by(date, country_region) %>% 
    summarise(infections = sum(infections)) %>% 
    ungroup
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # SIR
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta/N * I * S
      dI <- beta/N * I * S - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  
  
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((Infected - fit)^2)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #get input paramenters and generate forecast
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  Infected <- df_to_model$infections
  Day      <- 1:(length(Infected))
  N        <- country_population
  
  init <- c(S = N-Infected[1], I = Infected[1], R = 0)
  
  Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
  
  Opt_par <- setNames(Opt$par, c("beta", "gamma"))
  
  t   <- days_to_model # time in days
  fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Generate output df
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  output_df <- data.frame(
    date = seq(from = filter_from_date, to = (filter_from_date + max(t) - 1), by = "day"),
    forecast_infections = round(fit$I)
  ) %>% 
    full_join(df_to_model %>% select(-2), ., by = "date") %>% 
    rename(actual_infections = infections)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Output Stats
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
  
  height_of_pandemic <- output_df %>% 
    filter(forecast_infections == max(forecast_infections)) %>% pull(forecast_infections)
  
  height_of_pandemic_date <- output_df %>% 
    filter(forecast_infections == max(forecast_infections)) %>% pull(date)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # generate plots 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  full_forecast_plot <- output_df %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = forecast_infections / 1000000), colour = "blue") +
    geom_point(aes(y = actual_infections / 1000000), colour = "red") +
    
    geom_vline(xintercept = height_of_pandemic_date, line_type = "dotted") +
    
    annotate(geom = "text", 
             x = height_of_pandemic_date, 
             y = (height_of_pandemic / 1000000) / 2, 
             inherits = FALSE,
             label = str_glue("Max. infections of {comma(height_of_pandemic / 1000000, accuracy = 0.01)} mil on {height_of_pandemic_date}"), 
             color = "black", 
             angle = 90,
             vjust = -0.25,
             size = 4) +
    
    
    theme_bw() +
    xlab("") +
    ylab("Number of Infections - Millions") +
    scale_x_date(breaks = scales::pretty_breaks(n = 12))+   # for one year date labels
    scale_y_continuous(label = comma) +
    ggtitle(label = str_glue("{country_to_model} Corona Virus Infections Forecast"), 
            subtitle = "Blue Points are Forecast and Red Points are Actuals")
  # caption  = "Data: https://github.com/CSSEGISandData/COVID-19")
  
  
  blast_off_plot <- output_df %>% 
    slice(1:points_to_plot_in_short_plot) %>% 
    ggplot(aes(x = date)) +
    
    geom_line(aes(y = forecast_infections), colour = "blue") +
    geom_point(aes(y = forecast_infections), colour = "blue", shape = 3) +
    
    geom_point(aes(y = actual_infections), colour = "red") +
    theme_bw() +
    xlab("") +
    ylab("Number of Infections") +
    scale_x_date(breaks = scales::pretty_breaks(n = 12))+   # for one year date labels
    scale_y_continuous(label = comma) +
    ggtitle(label = str_glue("Initial Phase of Infection in {country_to_model} - First {points_to_plot_in_short_plot} forecast points vs. actual"), 
            subtitle = "Blue x are Forecast and Red Points are Actuals")
  # caption  = "Data: https://github.com/CSSEGISandData/COVID-19")
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Organise outputs
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  return(
    
    list(
      
      country_name            = country_to_model,
      full_forecast_plot      = full_forecast_plot,
      blast_off_plot          = blast_off_plot,
      Ro                      = R0,
      height_of_pandemic      = height_of_pandemic,
      height_of_pandemic_date = height_of_pandemic_date,
      Population              = country_population,
      forecast_data           = output_df
      
      
      
    ))
  
}

