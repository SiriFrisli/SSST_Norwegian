### After classifying the data it was found that a lot of the instances were not really related to COVID at all
# Therefore want to classify the data as relevant to teh padenmic or not
# Before running the SSST framework again

library(tidyverse)

covid <- readRDS("D:/Data/Datasets/covid_99_class_urls.RDS")

covid$date <- as.POSIXct(covid$created_at, format = "%Y-%m-%dT%H:%M:%S")
covid$date <- format(as.Date(covid$date), "%Y-%m")

# Keeping the first tweet from any conversation
covid <- covid[with(covid, order(date)), ]
first_tweet <- covid[match(unique(covid$conversation_id), covid$conversation_id), ]

first_tweet_grouped <- covid |>
  group_by(conversation_id) |>
  count()

sum(first_tweet_grouped$n) # adds up to the amount of total tweets

# Remove tweet that does not contain any of the query string
# (keep tweet that has a match in a link)
words <- c("korona", "covid", "covid 19", "corona", "pandemic", "pandemi", "koronavirus", "coronavirus", "sars cov 2")

covid_match <- first_tweet |>
  filter(
    str_detect(tweet, paste(words, collapse = "|")) |
      str_detect(url_1, paste(words, collapse = "|"))
    )

covid_no_match <- first_tweet |>
  filter(
    !str_detect(tweet, paste(words, collapse = "|")) & 
      (!str_detect(url_1, paste(words, collapse = "|")) | is.na(url_1))
  )

nrow(first_tweet) == nrow(covid_match) + nrow(covid_no_match)
# TRUE

# matching with the full dataset:
covid_full_match <- covid |>
  semi_join(covid_match, by = "conversation_id")
# 426 262

covid_full_no_match <- covid |>
  semi_join(covid_no_match, by = "conversation_id")
# 347 681

nrow(covid) == nrow(covid_full_match) + nrow(covid_full_no_match)
# TRUE

# Last check to see if how many of the words from the list is present in the no_match data
covid_no_match_full <- covid_full_no_match |>
  filter(
    str_detect(tweet, paste(words, collapse = "|")) |
      str_detect(url_1, paste(words, collapse = "|"))
  )
# 25 230 out of 347 681

covid_full_match |>
  ungroup() |>
  count(label)
# misinformation = 19 594
# non-misinformation = 406 668
# 0.045967


# want to check how many obs form the manually labelled sample is present in this new data
covid_man <- read_xlsx("D:/Data/Training samples/misinformation_labeled_finished.xlsx")
covid_man |>
  count(label)

covid_man_match <- covid_full_match |>
  semi_join(covid_man, by = "id")
# 994
# meaning there's 85 obs not present

covid_man_2 <- covid_man |>
  semi_join(covid_man_match, by = "id")

covid_man_2 |>
  count(label) 
# 920
# 74
# only 3 misinformation samples lost, that's decent

saveRDS(covid_full_match, "D:/Data/Datasets/Classification_data_filtered/covid_relevant_url.RDS")
saveRDS(covid_full_no_match, "D:/Data/Datasets/Classification_data_filtered/covid_not_relevant_url.RDS")

saveRDS(covid_man_2, "D:/Data/Training samples/misinformation_manual_labeled_filtered.RDS")

################################################################################
# also creating a saving one df without retweets, will be used for the classification

covid <- readRDS("D:/Data/Datasets/Classification_data_filtered/covid_relevant_url.RDS")

covid_nort <- covid |>
  filter(!str_detect(tweet, "^rt"))

saveRDS(covid_nort, "D:/Data/Datasets/Classification_data_filtered/covid_relevant_url_nort.RDS")
