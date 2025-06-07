#### Evaluation of the nowcasting models ####

library(rstan)
library(rstantools)
library(EpiNow2)
library(DBI)
library(RSQLite)
library(dplyr)

# Import functions
source("./code/2T_functions.r")
#source("./code/Y2_functions.r")

# Import data
dat <- read_csv("./data/covid_deaths.csv")
dat <- dat %>%
  select(death_date, n, rep_date) %>%
  mutate(death_date = if_else(is.na(death_date), lag(death_date) + 1, death_date))

dat <- dat %>% 
  filter(rep_date >= "2023-09-24", rep_date <= "2023-10-23")

# Restrict dataset to a specific nowcast dates
rep_dates <- list.files(path = paste0("./data/fohm/")) %>% 
  str_extract("\\d+-\\d+-\\d+") %>%  
  as.data.frame %>% 
  distinct() %>% 
  filter(. >= "2022-11-04", . <= "2022-12-29") %>%  
  t() %>%
  as.vector()

# Choose one of following models
models <- c("mod_r", "mod_r_cases", "mod_l", "mod_l_cases", 
            "mod_l_2", "mod_rl", "mod_rl_cases", "mod_rl_2")

Nrep_dates <- length(rep_dates)
Nrep_dates_2023 <- length(rep_dates_2023) 

for(i in 1:Nrep_dates){
  now <- rep_dates[i]
  model_spec <- "mod_r"
  lapply(model_spec , evaluate_nowcast, dat = dat, now = now)
}


