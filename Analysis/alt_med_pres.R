library(tidyverse)
library(lubridate)
library(dplyr)
library(urltools)
library(readxl)

# Loading and combining my data. One with classification labels, and one with domain names. 
covid <- readRDS("E:/Data/Datasets/Classification_data_filtered/covid_classified_without_rt_98_FINAL.RDS")

covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

covid$label <- case_when(
  covid$label == "non.misinfo" ~ "non misinformation",
  covid$label == "misinfo" ~ "misinformation")

# Creating a column with only the domain
covid_ext_dom <- sapply(covid$url_1, function (i) {
  x <- url_parse(i)
  c(x$domain)
})

covid_ext_dom <- data.frame(dom_url=covid_ext_dom,
                            id=covid$id)

covid_ext_dom$dom_url <- str_remove_all(covid_ext_dom$dom_url, "www.")

covid <- covid |>
  cbind(covid_ext_dom) |>
  select(-c(id...26)) |>
  rename(id = id...2)

covid_sites <- covid |>
  count(label, dom_url, sort = TRUE)

# Selecting the top 20 misinfo sites
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

covid_mis <- covid_sites |>
  filter(label == "misinformation") |>
  head(n = 22)
covid_mis <- covid_mis[-1,]
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

###
### Chi-square
###

am <- read_xlsx("~/SSST_Norwegian/anms.xlsx")
am <- am |>
  select(c("dom_url" = "domain", "language"))

covid_sites <- covid |>
  mutate(alt_news_link = ifelse(dom_url %in% am$dom_url, "yes", "no"))

covid_mis_sites <- covid_sites |>
  filter(label == "misinformation") |>
  drop_na(dom_url) |>
  count(dom_url,alt_news_link, sort = TRUE)

table(covid_mis_sites$alt_news_link)
# 1036 no, 14 yes.

cont_table <- table(covid_sites$label, covid_sites$alt_news_link)

chi_square_test <- chisq.test(cont_table)
print(chi_square_test)
# P < 0.05 (p-< 2.2e-16). There is an association between links to alternative news media sites and the misinformation label. 

###
### Odds ratio
###
library(epitools)
odds_ratio <- oddsratio(cont_table)
odds_ratio

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
# H5
covid_mis_sites <- covid_mis_sites[-1,]

covid_mis_sites$n_scaled <- scale_values(covid_mis_sites$n)

alt_news_plot <- ggplot(covid_mis_sites[1:10, ], aes(x = reorder(dom_url, n), y = n, fill = alt_news_link)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values=c( "#dfc27d", "#a6611a")) +
  labs(title = "Most Popular Domains in Misinformation Tweets",
       x = "Domain",
       y = "Count",
       fill = "Alternative News Site") +
  theme_minimal()

ggsave(filename = "FIGURE_4.png", plot = alt_news_plot, bg = "white", device = "png", units = "px", dpi="print")
