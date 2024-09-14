library(tidyverse)
library(lubridate)
library(dplyr)

# Loading and combining my data. One with classification labels, and one with domain names. 
covid <- readRDS("D:/Data/Datasets/Classification_datasets/misinformation_class_FINISHED_99.RDS")
covid_ext <- readRDS("D:/Data/Datasets/Classification_datasets/covid_relevant_nort_domain.RDS")

covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

covid <- covid |>
  merge(covid_ext, by = "id") |>
  select(tweet, month, id, created_at = created_at.x, author_hash = author_hash.x, label, dom_url)

covid$label <- case_when(
  covid$label == "non.misinfo" ~ "non misinformation",
  covid$label == "misinfo" ~ "misinformation")

covid_sites <- covid |>
  count(label, dom_url, sort = TRUE)

# Selecting the top 20 misinfo sites
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

covid_mis <- covid_sites |>
  filter(label == "misinformation") |>
  head(n = 21)
covid_mis <- covid_mis[-1,]

covid_mis$n_scaled <- scale_values(covid_mis$n)

misinfo_sites_bar <- covid_mis |>
  head(10) |>
  ggplot(aes(x = fct_reorder(dom_url, n_scaled), y = n_scaled)) +
  geom_col(fill="#F8766D") +
  coord_flip() + 
  labs(title = "Top 10 external sites among misinformation tweets", y = "Percentage", x = "") +
  theme(legend.position = "none")
misinfo_sites_bar

################################################################################
# Chi-square

am <- read.csv2("~/INORK_R/Processing/misinfo_sites.csv")
am <- am |>
  select(c("dom_url" = "url", "source"))

covid_sites <- covid |>
  mutate(alt_news_link = ifelse(dom_url %in% am$dom_url, "yes", "no"))

covid_mis_sites <- covid_sites |>
  filter(label == "misinformation") |>
  drop_na(dom_url) |>
  count(dom_url,alt_news_link, sort = TRUE)

table(covid_mis_sites$alt_news_link)
# 1030 no, 68 yes.

cont_table <- table(covid_sites$label, covid_sites$alt_news_link)

chi_square_test <- chisq.test(cont_table)
print(chi_square_test)
# P < 0.05 (p-< 2.2e-16). There is an association between links to alternative news media sites and the misinformation label. 

covid_alt_timeline <- covid_sites |>
  drop_na(dom_url) |>
  filter(label == "misinformation", alt_news_link == "yes") |>
  count(month, alt_news_link)
covid_alt_timeline$n_scaled <- scale_values(covid_alt_timeline$n)

covid_alt_timeline |>
  ggplot(aes(x = month, y = n)) +
  geom_line(linewidth = 1.2, color = "#F8766D") +
 # scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5)) +
  labs(y = "Counts", x = "", color = "")


################################################################################
# Alt news misinfo change
library(MASS)

misinfo_altnews_tweets <- covid_sites |>
  filter(label == "misinformation" & alt_news_link == "yes")

monthly_counts <- misinfo_altnews_tweets |>
  group_by(month) |>
  summarise(count = n())

monthly_counts <- monthly_counts |>
  mutate(time = as.numeric(month - min(month)))

poisson_model <- glm(count ~ time, data = monthly_counts, family = poisson)

summary(poisson_model)                     

ggplot(monthly_counts, aes(x = month, y = count)) +
  geom_line(linewidth = 1, color = "#F8766D") +
  # geom_point() +
  # geom_line(aes(y = predict(poisson_model, type = "response")), color = "red") +
  labs(title = "Misinformation Tweets with Links to Alternative News Sites",
       x = "Month",
       y = "Count") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5),
        legend.title = element_blank())
