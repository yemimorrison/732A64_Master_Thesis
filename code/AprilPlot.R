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
fohm_files_apr <- str_c(path, list.files(path))

# Get rep dates
rep_dates_apr <- fohm_files_apr %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  distinct() %>%
  # filter(. >= "2020-10-20", . <= "2021-05-21") %>%
  t() %>%
  as.vector()

names(fohm_files_apr) <- rep_dates_apr

# Import death data
covid_death_df_apr <- lapply(fohm_files_apr, read_excel,
                         sheet = 2, col_types = c("text", "numeric")
) %>%
  bind_rows(.id = "rep_date") %>%
  select(death_date = Datum_avliden, N = Antal_avlidna, rep_date) %>%
  filter(death_date != "Uppgift saknas") %>%
  mutate(
    death_date = as.Date(as.numeric(death_date), origin = "1899-12-30"),
    rep_date = as.Date(rep_date)
  )

specific_date <- as.Date("2020-04-15")

covid_death_df_apr_filtered <- covid_death_df_apr %>%
  filter(death_date == specific_date) %>%
  arrange(rep_date)  # Ensure it's sorted by case occurrence

covid_death_df_apr_filtered <- covid_death_df_apr_filtered %>%
  filter(rep_date >= as.Date("2020-04-15") & rep_date <= as.Date("2020-05-14"))

covid_death_df_apr_filtered %>%
write_csv("./data/covid_deaths_april.csv")

ggplot(covid_death_df_apr_filtered, aes(x = rep_date, y = N)) +
  geom_line(color = "red", size = 1.5) + 
  geom_point(color = "blue", size = 3) + 
  labs(
    title = paste("No of Deaths Reported Over Time for", specific_date),
    x = "Reported Date",
    y = "Number of deaths"
  ) +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 14),       # Axis title (x and y)
    axis.text = element_text(size = 12),        # Axis tick labels
    legend.title = element_text(size = 13),     # Legend title
    legend.text = element_text(size = 11)       # Legend item labels
  )


# Compute deaths n_t,d's
covid_death_df_apr %>%
  group_by(death_date) %>%
  mutate(n = c(first(N), diff(N))) %>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(death_date, n, rep_date) %>%
  mutate(death_date = if_else(is.na(death_date), lag(death_date) + 1, death_date))%>%
  write_csv("./data/covid_deaths.csv")

# Import ICU data
covid_icu_df <- lapply(fohm_files_apr, read_excel,
                       sheet = 3, col_types = c("text", "numeric")
) %>%
  bind_rows(.id = "rep_date") %>%
  select(icu_date = Datum_vårdstart, N = Antal_intensivvårdade, rep_date) %>%
  mutate(
    icu_date = as.Date(as.numeric(icu_date), origin = "1899-12-30"),
    rep_date = as.Date(rep_date)
  )

# Compute ICU n_{t,d}'s
covid_icu_df %>%
  group_by(icu_date) %>%
  mutate(n = c(first(N), diff(N))) %>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(icu_date, n, rep_date) %>%
  mutate(icu_date = if_else(is.na(icu_date), lag(icu_date) + 1, icu_date))%>%
  write_csv("./data/covid_icu.csv")
