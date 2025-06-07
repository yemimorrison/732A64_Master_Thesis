res_df <- read_csv("./data/daily_results.csv")

  # Make sure 'date' is in Date format
  library(lubridate)
  
  # Convert the 'date' column to Date format
  res_df$date <- mdy(res_df$date)
  
  # Plot
  rep_plot_a <- res_df %>%
    pivot_longer(c(med_r, n_true_retro)) %>%
    ggplot(aes(x = date)) +
    # Change line color and linetype
    geom_line(aes(y = value, color = name, linetype = name), size = 1) +
    # Change ribbon color and transparency
    geom_ribbon(aes(ymin = q5_r, ymax = q95_r), fill = "lightblue", alpha = 0.3) +
    # Define the color for the lines and their labels
    scale_color_manual(
      values = c("blue", "red"),  # Set line colors here
      labels = c("Nowcast (median)", "True no of cases")
    ) +
    # Define the linetype for the lines
    scale_linetype_manual(
      values = c(1, 2),  # Solid line for Nowcast, dashed for True number
      labels = c("Nowcast (median)", "True no of cases")
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
  
  rep_plot_a
  