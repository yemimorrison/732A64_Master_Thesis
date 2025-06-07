library(rstan)
library(rstantools)
library(EpiNow2)
library(DBI)
library(RSQLite)

# Connect to SQLite
con <- dbConnect(SQLite(), "fhmdata.sqlite")

# Load August 2023 data
query <- "SELECT Dag as event_date, Region, Fall_per_dag as cases, published_date as reported_date 
FROM acov19DAG WHERE Dag BETWEEN '2023-08-01' AND '2023-08-31'"

data <- dbGetQuery(con, query)

data$reported_date<- as.Date(as.character(data$reported_date), format="%Y%m%d")
data$event_date <- as.Date(data$event_date)
data$cases <- as.numeric(data$cases)

# Process data into time x delay matrix
T <- length(unique(data$event_date))
#D <- max(as.numeric(data$reported_date - data$event_date), na.rm = TRUE) 
D <- 31
y <- matrix(0, nrow = T, ncol = D) #Add +1 to D to deal with the issue of dimensions.

for (i in 1:nrow(data)) {
  t_idx <- as.numeric(data$event_date[i] - min(data$event_date)) + 1
  d_idx <- as.numeric(data$reported_date[i] - data$event_date[i]) + 1
  y[t_idx, d_idx] <- y[t_idx, d_idx] + data$cases[i]
}

# Load additional data (ICU admissions)
query_icu <- "SELECT Dag as event_date, sum(Intensivvårdade_respektive_avlidna_per_dag) as icu_cases_deaths FROM 
xcov19ivavDAG WHERE event_date BETWEEN '2023-08-01' AND '2023-08-31'
Group by Dag"

icu_data <- dbGetQuery(con, query_icu)

# Replace ".." with NA and convert column to numeric
icu_data <- icu_data %>%
  mutate(icu_cases_deaths = ifelse(icu_cases_deaths == "..", NA, icu_cases_deaths),
         icu_cases_deaths = as.numeric(icu_cases_deaths))  

m <- log(icu_data$icu_cases_deaths + 1)  # Avoid log(0)

# Set priors
stan_data <- list(
  T = T,
  D = D,
  y = y,
  sigma = 0.1,
  phi = 2.0,
  beta0 = 1.0,
  beta1 = 0.5,
  m = m
)

# Run Stan model
fit <- stan(file = "nowcasting_model.stan", data = stan_data, iter = 2000, chains = 4)

# View results
print(fit)
