# rawr::rawr("https://blog.ephorie.de/covid-19-the-case-of-germany", T)


library(deSolve)
library(platus)
library(tidyverse)
library(lubridate)

x <- util_scrape_table("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

y <- x %>% row_to_names(1) %>% clean_names %>% remove_empty() %>% 
  gather(date, infections, -1:-4) %>% 
  mutate(date = str_remove_all(date, "x")) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(infections = parse_number(infections))

from_date <-  dmy("12/03/2020")


oz    <- y %>% filter(country_region == "Australia") %>% 
  filter(date >= from_date) %>%
  group_by(date, province_state) %>% summarise(infections_oz = sum(infections)) %>% 
  filter(province_state == "New South Wales")  
# group_by(date) %>% summarise(infections_oz = sum(infections))  



Infected <- oz$infections_oz
Day <- 1:(length(Infected))
N <- 25635647 # AU population from ABS
N <- 8089500 # NSW

old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))
title("Total infections COVID-19 Australia", outer = TRUE, line = -2)


SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
##      beta     gamma 
## 0.6428120 0.3571881

t <- 1:80 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour

matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

points(Day, Infected)
legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model COVID-19 Australia", outer = TRUE, line = -2)


par(old)

R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
R0
##       R0 
## 1.799646

fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
##          I
## 54 9769398

max_infected <- max(fit$I)
max_infected / 5 # severe cases
## [1] 1953880

max_infected * 0.06 # cases with need for intensive care
## [1] 586163.9

# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
max_infected * 0.007 # deaths with supposed 0.7% fatality rate
## [1] 68385.78

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

data.frame(
  date = seq(from = from_date, to = (from_date + max(t) - 1), by = "day"),
  fcast_infections = round(fit$I),
  fcast_cum_icu    = round(fit$I * 0.06),
  fcast_cum_death  = round(fit$I * 0.007),
  fcast_recovery   = round(fit$R)
  
) %>% 
  full_join(oz %>% select(-2), .) %>% 
  
  xlopen



