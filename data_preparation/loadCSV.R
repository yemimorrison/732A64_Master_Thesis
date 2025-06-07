library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(data.table)
library(ff)

# Define folder path (modify as needed)
data_dir <- "C:/Users/HP/Documents/FHM_project/testdata/"

# List all daily folders (e.g., "20220401", "20220402", ...)
daily_folders <- list.dirs(data_dir, recursive = FALSE)

# Function to load a CSV file
load_csv <- function(file_path) {
  read_csv(file_path, col_types = cols(.default = "c")) %>% # Read everything as character
    mutate(Date = ymd(basename(dirname(file_path)))) # Extract date from folder name
}

# Function to load all files from a given folder
load_daily_data <- function(folder) {
  files <- list.files(folder, full.names = TRUE, pattern = "\\.csv$")
  data_list <- lapply(files, load_csv)
  bind_rows(data_list) # Merge all files into one dataframe
}


# Load all days
#raw_data <- bind_rows(lapply(daily_folders, load_daily_data))
#raw_data <- rbindlist(lapply(daily_folders, load_daily_data), use.names = TRUE, fill = TRUE)

chunk_size <- 20  # Number of files per batch
raw_data <- list()

for (i in seq(1, length(daily_folders), by = chunk_size)) {
  chunk <- daily_folders[i:min(i + chunk_size - 1, length(daily_folders))]
  chunk_data <- rbindlist(lapply(chunk, load_daily_data), use.names = TRUE, fill = TRUE)
  
  raw_data[[length(raw_data) + 1]] <- chunk_data
  rm(chunk_data)  # Remove chunk from memory
  gc()  # Free up memory
}

raw_datatest <- rbindlist(raw_data, use.names = TRUE, fill = TRUE)
rm(raw_data)
gc()


# Convert columns where needed
raw_data <- raw_data %>%
  mutate_at(vars(contains("Fall"), contains("Test"), contains("Intensivvårdade"), contains("Avlidna")), as.numeric) %>%
  mutate(Date = as.Date(Date))

nowcasting_data <- raw_data %>%
  filter(grepl("acov19DAG|xcov19ivavDAG", basename(file))) %>%
  select(Date, Cases = Fall.per.dag, ICU = Intensivvårdade, Deaths = Avlidna) %>%
  group_by(Region, Date) %>%
  summarise(across(Cases:Deaths, sum, na.rm = TRUE)) %>%
  arrange(Date)



gc()
memory.limit()

