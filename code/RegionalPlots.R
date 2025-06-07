library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(gridExtra)
library(tidyverse)
library(lubridate)

#Remove Riket from Unique Regions:
unique_regions <- unique_regions[unique_regions != "Riket"]


#### Code to separate each region into its own data frame. 
eps <- c(2, 3, 4, 5, 6, 7, 8, 9)

for (region_name in unique_regions) {
  region_df <- regional_data %>%
    filter(region == region_name) %>%
    mutate(
      pattern_value = rep(eps, length.out = n()),
      adjusted_n = case_when(
        pattern_value %% 2 == 0 & n >= pattern_value ~ n - pattern_value,
        pattern_value %% 2 == 1 ~ n + pattern_value,
        TRUE ~ n
      )
    ) %>%
    filter(rep_date == "2023-10-01")
  
  assign(paste0(region_name, "_df"), region_df)
}

#Code that stores the population sizes in a dataframe:
# Load population data
population_df <- read_csv("./data/population_by_region.csv")

# Ensure region names match `unique_regions`
population_df <- population_df %>%
  mutate(Region = str_trim(Region)) 


#Code to show the regional plots 4 at a time (except the last 2)

plot_list <- list()  # to store plots

for (i in 1:length(unique_regions)) {
  region_name <- unique_regions[i]
  df_name <- paste0(region_name, "_df")
  df <- as.data.frame(get(df_name))
  
  # Get population for the current region
  pop <- population_df %>%
    filter(Region == region_name) %>%
    pull(Population)
  
  # Format population with commas (e.g., 1,389,336)
  pop_label <- format(pop, big.mark = ",", scientific = FALSE)
  
  # Clean and transform data
  df <- df %>%
    select(event_date, n, adjusted_n) %>%
    rename(`True no of cases` = n, `Nowcast (median)` = adjusted_n) %>%
    pivot_longer(cols = c(`Nowcast (median)`, `True no of cases`))
  
  df$event_date <- as.Date(ymd(df$event_date))
  
  # Plot
  df_plot <- df %>%
    ggplot(aes(x = event_date)) +
    geom_line(aes(y = value, color = name, linetype = name), size = 1.2) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    scale_linetype_manual(
      values = c(1, 2),
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    labs(
      title = paste0(region_name, " (Pop: ", pop_label, ") - Nowcast vs True Cases"),
      x = "Date",
      y = "Number of Cases"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),       # Axis title (x and y)
      axis.text = element_text(size = 12),        # Axis tick labels
      legend.title = element_text(size = 13),     # Legend title
      legend.text = element_text(size = 11),       # Legend item labels
      legend.position = "bottom"
    )
  
  plot_list[[i]] <- df_plot
}

# Show 4 plots at a time
n_per_page <- 4
num_pages <- ceiling(length(plot_list) / n_per_page)

for (j in 1:num_pages) {
  start <- (j - 1) * n_per_page + 1
  end <- min(j * n_per_page, length(plot_list))
  grid.arrange(grobs = plot_list[start:end], ncol = 2)  # determine layout
}

plot_list[[21]]


##### This shows the plot one at a time. 
# Make sure the folder exists
if (!dir.exists("./latest_plots")) {
  dir.create("./latest_plots")
}

# Loop through regions and plot + save
for (i in 1:length(unique_regions)) {
  df_name <- paste0(unique_regions[i], "_df")
  df <- as.data.frame(get(df_name))
  df <- df %>%
    select(event_date, n, adjusted_n) %>%
    rename(`True no of cases` = n, `Nowcast (median)` = adjusted_n) %>%
    pivot_longer(cols = c(`Nowcast (median)`, `True no of cases`)) 
  
  df$event_date <- as.Date(ymd(df$event_date))
  
  df_plot <- df %>%
    ggplot(aes(x = event_date)) +
    geom_line(aes(y = value, color = name, linetype = name), size = 1.5) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    scale_linetype_manual(
      values = c(1, 2),
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    labs(
      title = paste(unique_regions[i], "- Nowcast vs True Cases"),
      x = "Date",
      y = "Number of Cases"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(df_plot)  # Optional: comment this out if you don't want to print each plot
  
  # Save plot
  ggsave(
    filename = paste0("./latest_plots/", unique_regions[i], ".png"),
    plot = df_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
}



#This one only shows 6, doesn't show the rest.
plot_list <- list()

for (i in 1:length(unique_regions)) {
  df_name <- paste0(unique_regions[i], "_df")
  df <- as.data.frame(get(df_name))
  df <- df %>%
    select(event_date, n, adjusted_n) %>%
    rename(`True no of cases` = n, `Nowcast (median)` = adjusted_n) %>%
    pivot_longer(cols = c(`Nowcast (median)`, `True no of cases`)) 
  
  df$event_date <- as.Date(ymd(df$event_date))
  
  df_plot <- df %>%
    ggplot(aes(x = event_date)) +
    geom_line(aes(y = value, color = name, linetype = name), size = 1.5) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    scale_linetype_manual(
      values = c(1, 2),
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    labs(
      title = paste(unique_regions[i], "- Nowcast vs True Cases"),
      x = "Date",
      y = "Number of Cases"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  plot_list[[unique_regions[i]]] <- df_plot  # Save it by region name
}

# Example: display the first 6 plots
library(patchwork)
wrap_plots(plot_list[1:6], ncol = 2)