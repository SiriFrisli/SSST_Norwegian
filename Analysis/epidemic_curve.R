library(tidyverse)

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)
################################################################################

covid_cases <- read.csv("~/SSST_Norwegian/covid_data_norway.csv") |>
  select(date, total_cases, new_cases, total_deaths, new_deaths, hosp_patients, total_tests, new_tests)

# The new cases are only updated weekly, I want to remove all rows where new cases are not reported
covid_cases_filtered <- covid_cases[-(which(covid_cases$new_cases %in% "0")), ]

# Formating the date column
covid_cases_filtered$month <- format(as.Date(covid_cases_filtered$date, 
                                            format = "%m/%d/%Y"), "%Y-%m")
covid_cases_filtered$month <- ym(covid_cases_filtered$month)
covid_cases_agg <- aggregate(covid_cases_filtered["new_cases"], by = covid_cases_filtered["month"], sum)

glimpse(covid_cases_filtered)

# Scaling the cases column
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
covid_cases_agg$cases_scaled <- scale_values(covid_cases_agg$new_cases)

covid_epidemic_plot <- covid_cases_agg |>
  ggplot(aes(x = month, y = cases_scaled)) +
  geom_col() +
  #scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
covid_epidemic_plot

################################################################################

covid_death_agg <- aggregate(covid_cases_filtered["new_deaths"], by = covid_cases_filtered["month"], sum)

# Scaling the deaths
covid_death_agg$deaths_scaled <- scale_values(covid_death_agg$new_deaths)

covid_deaths_plot <- covid_death_agg |>
  ggplot(aes(x = month, y = deaths_scaled)) +
  geom_col() +
  #scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
covid_deaths_plot

################################################################################
covid <- readRDS("D:/Data/misinformation_class_FINISHED_99.RDS")

covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

covid_filtered <- covid[-(which(covid$label %in% "non.misinfo")),]
covid_filtered$label <- case_when(covid_filtered$label == "misinfo" ~ "misinformation")

covid_date <- covid_filtered |>
  count(month, label, sort = TRUE)

covid_date$mis_scaled <- scale_values(covid_date$n)

covid_infodemic_plot <- covid_date |>
  ggplot(aes(x = month, y = mis_scaled)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
covid_infodemic_plot

legend <- c("confirmed deaths" = "#00bfc4", "confirmed cases" = "gray", "misinformation observations" = "#F8766D")

covid_ep_in_plot <- covid_death_agg |>
  ggplot(aes(x = month, y = deaths_scaled, fill = "label")) +
  geom_line(linewidth = 1.2, aes(color = "confirmed deaths")) +
  geom_line(data = covid_cases_agg, aes(y = cases_scaled, color = "confirmed cases"), linewidth = 1.2) +
  geom_line(data = covid_date, aes(y = mis_scaled, color = "misinformation observations"), linewidth = 1.2) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5)) +
  labs(y = "Normalized monthly incidence", x = "", color = "") +
  scale_color_manual(values = legend)
covid_ep_in_plot
