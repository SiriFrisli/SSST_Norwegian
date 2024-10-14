library(tidyverse)
library(readxl)
library(openxlsx)

covid <- readRDS("D:/Data/Datasets/Classification_data_filtered/covid_classified_without_rt_98_FINAL.RDS")

covid_nort <- covid |>
  filter(!str_detect(tweet, "^rt"))

set.seed(2)
covid_nort_sample <- covid_nort[sample(nrow(covid_nort), 300), ]

covid_nort_sample <- covid_nort_sample |>
  select(tweet, id, created_at, url_1)

write.xlsx(covid_nort_sample, "D:/Data/Training samples/misinformation_classified_98_sample.xlsx")

#################################################################################
library(irr)

covid_man <- read_xlsx("D:/Data/Training samples/misinformation_classified_98_sample.xlsx")

covid_man <- covid_man |>
  rename(label_man = label)

covid_clas <- covid  |>
  semi_join(covid_man, by = "id")

covid_clas <- covid_clas |>
  select(tweet, id, label_clas = label, url_1)
# covid_clas <- covid_clas[order(covid_clas$created_at, decreasing = TRUE),]

covid_kappa <- covid_man |>
  merge(covid_clas, by = "id") |>
  select(label_clas, label_man)

kappa_result_2 <- kappa2(covid_kappa, weight = "unweighted")
kappa_result_2
# 0.621 # from 98 thershold, with retweets during classification 
# 0.791 # from 98 threshold, no retweets during classification (final dataset)