# Load necessary libraries
library(tidyverse)
library(rstan)
library(lubridate)

# Example: loading regional data
#regional_data <- read.csv("./data/ccov19reg.csv")
colnames(regional_data)
# Rename specific columns using dplyr
library(dplyr)
regional_data <- regional_data %>%
  rename(year_week = `År och vecka`, region = Region, Indicator = Indikator, num_cases = `Bekräftade fall`)

# Prepare the data: Assume we have columns: region, year_week, num_cases
# Extract year and week from year_week

regional_data <- regional_data %>%
  mutate(year = as.numeric(substr(year_week, 1, 4)),
         week = as.numeric(substr(year_week, 7, 9)), # Extracting the week number
         event_date = case_when(
           week <= 52 ~ as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u"),
           week == 53 ~ as.Date(paste(year, 52, 1, sep = "-"), "%Y-%U-%u") + 6, # Handle week 53 as week 52 + 7 days
           TRUE ~ NA_Date_ # If the week is not valid, set as NA
         ))


# Remove the year_week column
regional_data <- regional_data %>%
  select(-Indicator)
# Remove the year_week column
regional_data <- regional_data %>%
  select(-year_week)

regional_data$num_cases <- as.numeric(regional_data$num_cases)

regional_data <- regional_data %>% 
  filter(event_date >= "2023-04-01")

# Compute confirmed cases n_t,d's
regional_data <- regional_data %>%
  group_by(event_date) %>%
  mutate(n = c(first(num_cases), diff(num_cases)))%>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(region, event_date, n, rep_date) %>%
  mutate(event_date = if_else(is.na(event_date), lag(event_date) + 1, event_date))

unique_regions <- unique(regional_data$region)

regional_data <- regional_data %>%
  group_by(region, event_date, rep_date) %>%
  summarise(N = sum(n, na.rm = TRUE), .groups = "drop")

regional_data <- regional_data %>%
  filter(region == "Riket")

regional_data <- regional_data %>%
  filter(rep_date >= "2023-05-01", rep_date<="2023-05-31")


# Create a time index for each region's time-series
regional_data <- regional_data %>%
  group_by(region) %>%
  mutate(time_index = row_number())

# Create a list for Stan modeling
data_list <- list(
  num_regions = length(unique(regional_data$region)),
  num_weeks = nrow(regional_data),
  region = as.integer(factor(regional_data$region)),
  time_index = regional_data$time_index,
  num_cases = regional_data$n
)

# Define the Stan model (simplified example of a hierarchical time-series model)
stan_model_code <- "
data {
  int<lower=0> num_regions;
  int<lower=0> num_weeks;
  int<lower=1, upper=num_regions> region[num_weeks];
  int<lower=1> time_index[num_weeks];
  vector<lower=0>[num_weeks] num_cases;
}

parameters {
  real alpha[num_regions];  // region-specific intercept
  real beta[num_regions];   // region-specific slope
  real<lower=0> sigma[num_regions];  // region-specific noise
  real mu_alpha;  // overall intercept
  real mu_beta;   // overall slope
  real<lower=0> sigma_alpha;  // variance of alpha
  real<lower=0> sigma_beta;   // variance of beta
}

model {
  for (i in 1:num_weeks) {
    num_cases[i] ~ normal(alpha[region[i]] + beta[region[i]] * time_index[i], sigma[region[i]]);
  }
  
  // Hierarchical prior for the region-specific intercept and slope
  alpha ~ normal(mu_alpha, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta);
  
  // Priors for the overall parameters
  mu_alpha ~ normal(0, 10);
  mu_beta ~ normal(0, 10);
  sigma_alpha ~ normal(0, 10);
  sigma_beta ~ normal(0, 10);
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_model_code)

# Fit the model
fit <- sampling(stan_model, data = data_list, iter = 2000, warmup = 1000)

# Print the results
print(fit)

# Optionally, extract the results
alpha_samples <- extract(fit)$alpha
beta_samples <- extract(fit)$beta

# Extract sigma (region-specific noise)
sigma_samples <- extract(fit)$sigma

# Extract overall parameters
mu_alpha_samples <- extract(fit)$mu_alpha
mu_beta_samples <- extract(fit)$mu_beta

# Get a summary of the fit
print(summary(fit))

# Plot the posterior distribution of alpha (region-specific intercept)
alpha_df <- as.data.frame(alpha_samples)
ggplot(alpha_df, aes(x = V1)) +
  geom_density() +
  theme_minimal() +
  labs(title = "Posterior Distribution of Alpha (Intercepts)")

# Plot the posterior distribution of beta (region-specific slopes)
beta_df <- as.data.frame(beta_samples)
ggplot(beta_df, aes(x = V1)) +
  geom_density() +
  theme_minimal() +
  labs(title = "Posterior Distribution of Beta (Slopes)")

# Generate predictions using posterior samples
predictions <- extract(fit)$alpha + extract(fit)$beta * regional_data$time_index

# Compute RMSE for model evaluation
rmse <- sqrt(mean((predictions - regional_data$n)^2))
print(paste("RMSE:", rmse))

