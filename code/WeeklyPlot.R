#### Plots and tables ####

# Load packages and functions
source("./code/2T_functions.r")

loadfonts(device = "win")

# Make sure 'date' is in Date format
library(lubridate)

# Import COVID data and Nowcast results
dat <- read_csv("./data/covid_deaths.csv")
#res_df2 <- err_df
res_df2 <- read_csv("./results/summarized_results_and_tables/results.csv")

res_df2$date <- as.Date(res_df2$date)

#%>% filter(date >= "2022-09-29")
# Convert the 'date' column to Date format
res_df2$date <- mdy(res_df2$date)


#2022 weekly data
rep_plot_b <- res_df2 %>%
  pivot_longer(c(med_r, n_true_retro)) %>%
  ggplot(aes(x = date)) +
  # Change line color and linetype
  geom_line(aes(y = value, color = name, linetype = name), size = 1) +
  # Change ribbon color and transparency
  geom_ribbon(aes(ymin = q5_r, ymax = q95_r), fill = "pink", alpha = 0.3) +
  # Define the color for the lines and their labels
  scale_color_manual(
    values = c("blue", "red"),  # Set line colors here
    labels = c("Nowcast (median)", "True number")
  ) +
  # Define the linetype for the lines
  scale_linetype_manual(
    values = c(1, 2),  # Solid line for Nowcast, dashed for True number
    labels = c("Nowcast (median)", "True number")
  ) +
  labs(
    title = "Nowcast vs True Cases",
    x = "Date",
    y = "Number of Cases"
  ) +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 14),       # Axis title (x and y)
    axis.text = element_text(size = 12),        # Axis tick labels
    legend.title = element_text(size = 13),     # Legend title
    legend.text = element_text(size = 11)       # Legend item labels
  )

rep_plot_b
