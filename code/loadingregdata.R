library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(data.table)
library(ff)

# Define folder path (modify as needed)
data_dir <- "./data/fohm/fhmdata/"

# List all daily folders (e.g., "20220401", "20220402", ...)
daily_folders <- list.dirs(data_dir, recursive = FALSE)

# Function to load a CSV file
load_csv <- function(file_path) {
  read_csv(file_path, col_types = cols(.default = "c")) %>% # Read everything as character
    mutate(rep_date = ymd(basename(dirname(file_path)))) # Extract date from folder name
}


# Extract just the folder names (e.g., "20230101") and store as a vector
rep_dates_2023 <- basename(daily_folders)
rep_dates_2023 <- rep_dates_2023[grepl("^2023", rep_dates_2023)]
rep_dates_2023 <- as.Date(rep_dates_2023, format = "%Y%m%d")
rep_dates_2023 <- as.data.frame(rep_dates_2023)
rep_dates_2023 <- rep_dates_2023 %>% 
  distinct() %>% 
  filter(. >= "2023-09-24", . <= "2023-12-22") %>%  
  t() %>%
  as.vector()

# Function to load only 'ccov19Reg.csv' files from a given folder
load_daily_data <- function(folder) {
  # List files and filter for 'ccov19Reg.csv'
  files <- list.files(folder, full.names = TRUE, pattern = "xcov19ivavDAG.csv$")
  data_list <- lapply(files, load_csv)
  bind_rows(data_list) # Merge all files into one dataframe
}

# Function to load only 'ccov19Reg.csv' files from a given folder
load_daily_data2 <- function(folder) {
  # List files and filter for 'ccov19Reg.csv'
  files <- list.files(folder, full.names = TRUE, pattern = "ccov19Reg.csv$")
  data_list <- lapply(files, load_csv)
  bind_rows(data_list) # Merge all files into one dataframe
}

# Example: Apply to all daily folders
all_data <- lapply(daily_folders, load_daily_data2) %>% bind_rows()

regional_data <- all_data

#regional_data <- all_data
colnames(all_data)
all_data <- all_data%>%
  rename(Indicator = Indikator, death_date = Dag, N = `Intensivvårdade respektive avlidna per dag`)

all_data <- all_data %>%
  mutate(N = as.numeric(str_replace_all(N, fixed(".."), "0")))

all_data <- all_data %>%
  select(-Indicator)

all_data <- all_data %>%
  mutate(
    death_date = as.Date(death_date),
    rep_date = as.Date(rep_date)
  )

all_data %>%
  group_by(death_date) %>%
  mutate(n = c(first(N), diff(N))) %>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(death_date, n, rep_date) %>%
  mutate(death_date = if_else(is.na(death_date), lag(death_date) + 1, death_date))%>%
  write_csv("./data/covid_deaths_2023.csv")
