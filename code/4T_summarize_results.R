#### This document is for summarizing the nowcasting results
#### used for plots and tables.

# Load packages and functions
source("./code/2T_functions.r")

## Import data
dat <- read_csv("./data/covid_deaths.csv")

## Restrict dataset to a specific nowcast date
rep_dates <- list.files(path = paste0("./data/fohm/")) %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  filter(. >= "2020-10-31") %>%
  distinct() %>%
  t() %>%
  as.vector()

retro_truth <- dat %>%
  group_by(date = death_date) %>%
  summarise(n_true_retro = sum(n))

list_files <- function(parameter, mod) {
  files <- list.files(path = paste0("./results/", parameter))
  files[str_detect(files, mod)]
}

## List files
files_mod_r <- list_files("N", "mod_r")


rep_dates <- files_mod_r %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  filter(. >= "2020-10-31") %>%
  distinct() %>%
  t() %>%
  as.vector()


## Read files
N_mod_r <- lapply(paste0("./results/N/", files_mod_r), read_csv)

N_r_df <- med_and_quantiles(N_mod_r)

Nres_mod_r <- calculate_scores(N_mod_r)

## Table 1
tbl1 <- bind_rows(
  rsme = c(
    Nres_mod_r$rmse %>% mean()
    
  ),
  logs = c(
    Nres_mod_r$logs %>% mean()
  ),
  crps = c(
    Nres_mod_r$crps %>% mean()
  ),
  pi_75 = c(
    Nres_mod_r$pi_75 %>% mean()
  ),
  pi_90 = c(
    Nres_mod_r$pi_90 %>% mean()
  ),
  pi95 = c(
    Nres_mod_r$pi_95 %>% mean()
  )
)

write_csv(tbl1, "./results/summarized_results_and_tables/table1.csv")


## Create results table
max_delay <- 0
err_df <- N_r_df %>%
  filter(delay == max_delay) %>%
  select(date, med_r = med, q5_r = q5, q95_r = q95) %>%
  left_join(dat %>% group_by(date = death_date) %>%
    summarise(n_true_retro = sum(n))) %>%
  mutate(
    err_r_7 = apply(Nres_mod_r$rmse, 1, mean),
    log_r_7 = apply(Nres_mod_r$logs, 1, mean),
    crps_r_7 = apply(Nres_mod_r$crps, 1, mean),
    rmse_r_7 = apply(Nres_mod_r$rmse, 1, mean)
  )

write_csv(err_df, "./results/summarized_results_and_tables/results.csv")


## Decreasing score
mod_r_all <- calculate_scores(N_mod_r, 35)


err_by_delay_df <- tibble(
  delay = 0:35,
  crps_r = mod_r_all$crps %>% apply(2, mean),
  #crps_a = mod_a_all$crps %>% apply(2, mean),
  #crps_b = mod_b_all$crps %>% apply(2, mean),
  #crps_d = mod_d_all$crps %>% apply(2, mean),
  logs_r = mod_r_all$logs %>% apply(2, mean),
  #logs_a = mod_a_all$logs %>% apply(2, mean),
  #logs_b = mod_b_all$logs %>% apply(2, mean),
  #logs_d = mod_d_all$logs %>% apply(2, mean),
  rmse_r = mod_r_all$rmse %>% apply(2, mean)
  #rmse_a = mod_a_all$rmse %>% apply(2, mean),
  #rmse_b = mod_b_all$rmse %>% apply(2, mean),
  #rmse_d = mod_d_all$rmse %>% apply(2, mean)
)

write_csv(err_by_delay_df, "./results/summarized_results_and_tables/error_by_delay.csv")

## Beta_0 for mod l
files_mod_b <- list_files("beta_0", "mod_b")

# Read files
beta_0_mod_b <- lapply(paste0("../results/beta_0/", files_mod_b), read_csv)
beta_0_df <- med_and_quantiles(beta_0_mod_b)
max_delay <- 0
beta_0_df <- beta_0_df %>% filter(delay == max_delay)
write_csv(beta_0_df, "../results/summarized_results_and_tables/results_beta_0.csv")


## Beta_1 for mod l and rl
files_mod_b <- list_files("beta_1", "mod_b")
files_mod_d <- list_files("beta_1", "mod_d")

# Read files
beta_1_mod_b <- lapply(paste0("../results/beta_1/", files_mod_b), read_csv)
beta_1_mod_d <- lapply(paste0("../results/beta_1/", files_mod_d), read_csv)

# Create results beta table
beta_1_df <- med_and_quantiles(beta_1_mod_b)
write_csv(
  beta_1_df %>% filter(delay == max_delay),
  "../results/summarized_results_and_tables/results_beta_1_mod_b.csv"
)

beta_1_df_d <- N_d_df %>% filter(delay == max_delay)
write_csv(beta_1_df_d, "../results/summarized_results_and_tables/results_beta_1_mod_d.csv")



## Table S2
path_summary <- "./results/summary/"
files_summary <- list.files(path_summary)
files_summary <- files_summary[str_detect(files_summary, "2020-11")]
summary_list <- lapply(path_summary %>% paste0(files_summary), read_csv)

# Running times
times <- c()
for (l in 1:length(files_summary)) {
  times[l] <- summary_list[[l]]$run_time[1]
}

path_N <- "./results/N/n_" %>% paste0(files_summary)
path_N <- str_remove(path_N, "placeholder")

N_list <- lapply(path_N, read_csv)

res_tabS2 <- calculate_scores(N_list = N_list, rep_date = rep("2020-12-22", length(N_list)))

# Collecting results
tabS2 <- bind_cols(
  model = files_summary %>% str_remove(".csv"),
  crps_7 = res_tabS2$crps %>% apply(1, mean),
  logs_7 = res_tabS2$logs %>% apply(1, mean),
  rmse_7 = res_tabS2$rmse %>% apply(1, mean),
  PI_75 = res_tabS2$pi_75 %>% apply(1, mean),
  PI_90 = res_tabS2$pi_90 %>% apply(1, mean),
  PI_95 = res_tabS2$pi_95 %>% apply(1, mean),
  running_times = times * 60
) %>% as.data.frame()

tabS2 %>% write_csv("./results/summarized_results_and_tables/tableS2.csv")

### Table S3

# Extract rep dates for the comparison
rep_dates_s3 <- rep_dates[(1:11) * 10]

files_mod_r <- str_c("./results/N/N_mod_a_ph_", rep_dates_s3, ".csv")
files_mod_r2 <- str_c("./results/N/N_mod_r2_", rep_dates_s3, ".csv")

N_list_s3 <- lapply(c(files_mod_r, files_mod_r2), read_csv)
scores_s3 <- calculate_scores(N_list = N_list_s3, rep_date = c(rep_dates_s3, rep_dates_s3))

T_s3 <- bind_cols(
  model  = c(rep("mod_r", 11), rep("mod_r2", 11)),
  crps_7 = scores_s3$crps %>% apply(1, mean),
  logs_7 = scores_s3$logs %>% apply(1, mean),
  rmse_7 = scores_s3$rmse %>% apply(1, mean),
  PI_75  = scores_s3$pi_75 %>% apply(1, mean),
  PI_90  = scores_s3$pi_90 %>% apply(1, mean),
  PI_95  = scores_s3$pi_95 %>% apply(1, mean)
) %>%
  as.data.frame() %>%
  group_by(model) %>%
  summarise(
    crps = mean(crps_7), logs = mean(logs_7), rmse = mean(rmse_7),
    PI_75 = mean(PI_75), PI_90 = mean(PI_90), PI_95 = mean(PI_95)
  )

# Write table
write_csv(T_s3, "./results/summarized_results_and_tables/tableS3.csv")


## Empirical reporting probability and quantiles

# Functuin for quantiles 
rep_quant <- function(p_df, q = 0.5){
  p_df %>%
    filter(cum_frac >= q) %>%
    group_by(death_date) %>%
    filter(delay == min(delay))
}

# Import model estimates
p_est_a_1230 <- read_csv("./results/p/p_mod_a_ph_2020-12-30.csv")
p_est_d_1230 <- read_csv("./results/p/p_mod_b_cp_2020-12-30.csv")
p_est_b_1230 <- read_csv("./results/p/p_mod_d_new2_2020-12-30.csv")

# Emprical quantiles from observed data
p_1230_emp <- dat %>%
  mutate(delay = as.numeric(rep_date - death_date)) %>%
  filter(death_date >= "2020-11-01", death_date <= "2020-12-30") %>%
  group_by(death_date, delay) %>%
  summarise(n = sum(n)) %>%
  right_join(tibble(expand.grid(
    death_date = seq(as.Date("2020-11-25"), as.Date("2020-12-30"), "1 day"),
    delay = 0:500
  ))) %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(death_date, delay) %>%
  mutate(frac = n / sum(n), cum_frac = cumsum(frac))

q50_emp <- rep_quant(p_1230_emp, 0.5) %>% 
  select(death_date, q_5_emp = delay)

q05_emp <- rep_quant(p_1230_emp, 0.05)  %>%
  select(death_date, q_05_emp = delay)

q95_emp <- rep_quant(p_1230_emp, 0.95) %>%
  select(death_date, q_95_emp = delay)


# Estimated reporting probability
est_rep_prob <- function(p_df, mod = "Model"){
  p_est <- p_df %>%
    pivot_longer(starts_with("p")) %>%
    group_by(name) %>%
    summarise(med = mean(value)) %>%
    mutate(
      day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
      delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3])) - 1
    ) %>%
    arrange(day, delay) %>%
    mutate(death_date = ymd("2020-12-30") - (56 - day)) %>%
    select(death_date, delay, est_p = med) %>%
    group_by(death_date) %>%
    mutate(cum_frac = cumsum(est_p))
  
  q50_est <- rep_quant(p_est, 0.5) %>%
    select(death_date, q_5_est = delay)
  
  q05_est <- rep_quant(p_est, 0.05) %>% 
    select(death_date, q_05 = delay)
  
  q95_est <- rep_quant(p_est, 0.95) %>% 
    select(death_date, q_95 = delay)
  
  q50_emp %>%
    left_join(q05_emp) %>%
    left_join(q95_emp) %>%
    left_join(q50_est) %>%
    left_join(q05_est) %>%
    left_join(q95_est) %>%
    pivot_longer(c(q_5_emp:q_95)) %>%
    mutate(name = factor(name, levels = c("q_05", "q_05_emp", "q_5_est", "q_5_emp", "q_95", "q_95_emp")),
           model = mod)
  
}

# Combine results
q_plot_df <- bind_rows(est_rep_prob(p_est_a_1230, "r"),
                        est_rep_prob(p_est_b_1230, "l"), 
                        est_rep_prob(p_est_d_1230, "rl"))
# Save results
write_csv(q_plot_df, "./results/summarized_results_and_tables/q_plot.csv")


## Empirical probability plot 

bind_rows(
  p_est_a_1230 %>%
    pivot_longer(starts_with("p")) %>%
    group_by(name) %>%
    summarise(med = mean(value)) %>%
    mutate(
      day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
      delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3])) - 1
    ) %>%
    arrange(day, delay) %>%
    mutate(death_date = ymd("2020-12-30") - (56 - day)) %>%
    select(death_date, delay, est_p = med) %>%
    group_by(death_date) %>%
    mutate(cum_frac = cumsum(est_p)) %>% mutate(mod = "r"),
  p_est_b_1230 %>%
    pivot_longer(starts_with("p")) %>%
    group_by(name) %>%
    summarise(med = mean(value)) %>%
    mutate(
      day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
      delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3])) - 1
    ) %>%
    arrange(day, delay) %>%
    mutate(death_date = ymd("2020-12-30") - (56 - day)) %>%
    select(death_date, delay, est_p = med) %>%
    group_by(death_date) %>%
    mutate(cum_frac = cumsum(est_p)) %>% mutate(mod = "l"),
  p_est_d_1230 %>%
    pivot_longer(starts_with("p")) %>%
    group_by(name) %>%
    summarise(med = mean(value)) %>%
    mutate(
      day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
      delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3])) - 1
    ) %>%
    arrange(day, delay) %>%
    mutate(death_date = ymd("2020-12-30") - (56 - day)) %>%
    select(death_date, delay, est_p = med) %>%
    group_by(death_date) %>%
    mutate(cum_frac = cumsum(est_p)) %>% mutate(mod = "rl"),
  dat %>%
    mutate(delay = as.numeric(rep_date - death_date)) %>%
    filter(death_date >= "2020-11-01", death_date <= "2020-12-30") %>%
    group_by(death_date, delay) %>%
    summarise(n = sum(n)) %>%
    right_join(tibble(expand.grid(
      death_date = seq(as.Date("2020-11-25"), as.Date("2020-12-30"), "1 day"),
      delay = 0:500
    ))) %>%
    mutate(n = replace_na(n, 0)) %>%
    arrange(death_date, delay) %>%
    mutate(frac = n / sum(n), cum_frac = cumsum(frac)) %>% 
    select(death_date, delay, est_p = frac, cum_frac) 
)  %>% write_csv("./results/summarized_results_and_tables/p_plot.csv")  
