#### This document is for computing the elements of the reporting triangle of COVID-19 deaths
#### reported by the Swedish Public Health Agency Folkhälsomyndigheten (fohm)

# Libraries
# if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, data.table, lubridate, readxl, readr, zoo, splitstackshape, stringr
)

# File path
path <- "./data/fohm/"

# Get file names from fohm
fohm_files <- str_c(path, list.files(path))

# Get rep dates
rep_datesx <- fohm_files %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  distinct() %>%
  # filter(. >= "2020-10-20", . <= "2021-05-21") %>%
  t() %>%
  as.vector()

names(fohm_files) <- rep_datesx

# Import  data
covid_cases_df2 <- lapply(fohm_files, read_excel,
                          sheet = 1, col_types = c("text", "numeric","numeric","numeric","numeric",
                                                   "numeric","numeric","numeric","numeric","numeric",
                                                   "numeric","numeric","numeric","numeric","numeric",
                                                   "numeric","numeric","numeric","numeric","numeric",
                                                   "numeric","numeric","numeric")
) %>%
  bind_rows(.id = "rep_date") %>%
  select(event_date = Statistikdatum, N = Totalt_antal_fall, rep_date) %>%
  filter(event_date != "Uppgift saknas") %>%
  mutate(
    event_date = as.Date(as.numeric(event_date), origin = "1899-12-30"),
    rep_date = as.Date(rep_date)
  )

# Compute n_t,d's
covid_cases2 <- covid_cases_df2 %>%
  group_by(event_date) %>%
  mutate(n = c(first(N), diff(N))) %>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(event_date, n, rep_date) 



# Get the latest total reported cases for each event_date
total_cases <- covid_cases_df2 %>%
  group_by(event_date) %>%
  summarise(cases_occurred = max(N, na.rm = TRUE))  # True total cases that occurred

# Compute cases still unreported
covid_cases_unreported <- covid_cases2 %>%
  left_join(total_cases, by = "event_date") %>%
  group_by(event_date, rep_date) %>%
  summarise(
    reported_so_far = sum(n, na.rm = TRUE),   # Sum of reported cases up to that rep_date
    cases_occurred = max(cases_occurred, na.rm = TRUE)
  ) %>%
  mutate(cases_unreported = cases_occurred - reported_so_far) %>%
  filter(cases_unreported > 0)


final_cases <- covid_cases_unreported %>%
  group_by(event_date) %>%
  slice_max(reported_so_far, with_ties = FALSE) %>%
  select(event_date, rep_date, reported_so_far, cases_occurred, cases_unreported)


final_cases_filtered <- final_cases %>%
  filter(event_date >= as.Date("2020-04-11") & event_date <= as.Date("2020-04-21"))


final_cases_filtered %>% write_csv("./data/final_cases.csv")
  


library(ggplot2)

# Convert data to long format for ggplot
final_cases_long <- final_cases_filtered %>%
  pivot_longer(cols = c(reported_so_far, cases_unreported),
               names_to = "status",
               values_to = "cases")

# Create the bar plot
ggplot(final_cases_long, aes(x = event_date, y = cases, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("reported_so_far" = "blue", "cases_unreported" = "grey"),
                    labels = c("Occurred but not yet reported", "Reported")) +
  labs(x = "Date", y = "Number of Cases",
       fill = NULL, title = "COVID-19 Reported vs. Unreported Cases") +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 14),       # Axis title (x and y)
    axis.text = element_text(size = 12),        # Axis tick labels
    legend.title = element_text(size = 13),     # Legend title
    legend.text = element_text(size = 11)       # Legend item labels
  )

