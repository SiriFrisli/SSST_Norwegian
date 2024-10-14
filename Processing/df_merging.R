library(tidyverse)

# Want to combine the datasets with RTs
covid_all <- readRDS("D:/Data/Datasets/covid_norwegian_media.rds")
covid <- readRDS("D:/Data/Datasets/Classification_datasets/misinformation_class_FINISHED_99.RDS")

# Adding the engagement metrics to the covid df from the covid_all df
covid <- covid |>
  merge(covid_all, by = c("id", "conversation_id", "created_at")) |>
  mutate(author_hash = coalesce(author_hash.x, author_hash.y),
          in_reply_to_user_hash = coalesce(in_reply_to_user_hash.x, in_reply_to_user_hash.y)) |>
  select(referenced_tweets, tweet, label, id, author_hash, in_reply_to_user_hash, conversation_id, created_at, retweet_count, urls,
         reply_count, like_count, quote_count, impression_count, .pred_misinfo, .pred_non.misinfo)

covid_all <- covid_all |>
  rename(tweet = text) |>
  filter(created_at >= "2020-01-01")

# Removing tweets from covid_all that already are in covid
covid_all <- covid_all |>
  anti_join(covid, by = "id")

removeURL <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "", tweet))
}

covid_all$tweet <- apply(covid_all["tweet"], 1, removeURL)
covid_all$tweet <- apply(covid_all["tweet"], 1, removeUsernames)

covid_all$tweet <- tolower(covid_all$tweet)
covid_all$tweet <- gsub("[[:punct:]]", " ", covid_all$tweet)

#################################################################################

# Create a new column in covid_all for matching purposes
covid_all <- covid_all |>
  mutate(tweet_match = sub("^rt", "", tweet))

# Need to match the dfs based on the Id_variable in the list referenced_tweets
covid_all_retweets <- covid_all |>
  mutate(unnest_referenced_tweets = referenced_tweets) |>
  unnest(unnest_referenced_tweets, names_sep = "_") |>
  filter(unnest_referenced_tweets_type == "retweeted")

# Merge the dataframes based on the new text_match column
covid_merged <- covid_all_retweets |>
  left_join(covid, by = c("unnest_referenced_tweets_id" = "id")) |>
    mutate(tweet = coalesce(tweet.x, tweet.y),
           conversation_id = coalesce(conversation_id.x, conversation_id.y),
           created_at = coalesce(created_at.x, created_at.y),
           retweet_count = coalesce(retweet_count.x, retweet_count.y),
           reply_count = coalesce(reply_count.x, reply_count.y),
           like_count = coalesce(like_count.x, like_count.y),
           quote_count = coalesce(quote_count.x, quote_count.y),
           impression_count = coalesce(impression_count.x, impression_count.y),
           referenced_tweets = coalesce(referenced_tweets.x, referenced_tweets.y),
           author_hash = coalesce(author_hash.x, author_hash.y),
           urls = coalesce(urls.x, urls.y),
           in_reply_to_user_hash = coalesce(in_reply_to_user_hash.x, in_reply_to_user_hash.y)) |>
    select(tweet, label, id, referenced_tweets, author_hash, in_reply_to_user_hash, conversation_id, created_at, urls, retweet_count,
           reply_count, like_count, quote_count, impression_count, .pred_misinfo, .pred_non.misinfo)

covid_merged_na <- covid_merged |>
  filter(is.na(label))

covid_merged <- covid_merged |>
  filter(!is.na(label))

covid_merged_full <- covid_merged |>
  rbind(covid) |>
  arrange(created_at)

saveRDS(covid_merged_full, "D:/Data/Datasets/covid_processed_class99_rt.RDS")

################################################################################

# NEW
## Comparing the classified datasets, choosing one, and merging with the retweets data
# without retweets
nort_98 <- readRDS("D:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_19_98_nort.RDS")
nort_98_not_labeled <- readRDS("D:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_ALL_98.RDS")

nort_98_not_labeled <- nort_98_not_labeled |>
  select(-c(label, .pred_misinfo...14, .pred_non.misinfo...15, .pred_misinfo...25, .pred_non.misinfo...26)) |>
  anti_join(nort_98, by = "id")

# with retweets
rt_98 <- readRDS("D:/Data/Training samples/st_log_reg_98_filtered/misinformation_class_19_98.RDS")
rt_98 <- rt_98 |>
  select(-c(pred_misinfo, pred_non_misinfo))

# Creating a dataset with id matches, to compare if the label is the same
matches <- rt_98 |>
  inner_join(nort_98, by = "id")

mismatched_labels <- matches |>
  filter(label.x != label.y)
# 1129 out of 176033

mismatched_labels |>
  count(label.x)
# misinfo = 1051, non.misinfo = 78

mismatched_labels |>
  count(label.y)
# misinfo = 78, non.misinfo = 1051
# it seems nort_98 is more accurate, with fewer misinfo labels. 

################################################################################
# combines the nort_98 with the nort_98_not_labeled
# first create a labeled=="non.misinfo" column in nort_98_not_labeled
nort_98_not_labeled$label <- "non.misinfo"

# then combine the two dfs
nort_98_combined <- nort_98_extra |>
  rbind(nort_98_not_labeled)

################################################################################
# Adds the retweets back into nort_98_combined
covid <- readRDS("D:/Data/Datasets/Classification_data_filtered/covid_relevant_url.RDS")

covid <- covid |>
  select(-c(label, .pred_misinfo, .pred_non.misinfo))

nort_98_extra <- nort_98 |>
  inner_join(covid, by = "id") |>
  mutate(tweet = coalesce(tweet.x, tweet.y)) |>
  select(-c(tweet.x, tweet.y))

# Need to match the dfs based on the Id_variable in the list referenced_tweets
covid_retweets <- covid |>
  mutate(unnest_referenced_tweets = referenced_tweets) |>
  unnest(unnest_referenced_tweets, names_sep = "_") |>
  filter(unnest_referenced_tweets_type == "retweeted") |>
  select(-unnest_referenced_tweets_type)

# checking that the length of the dfs adds up
nrow(nort_98_combined) + nrow(covid_retweets) == nrow(covid)
# TRUE

covid_retweets_with_label <- covid_retweets |>
  left_join(nort_98_combined[, c("id", "label")], 
            by = c("unnest_referenced_tweets_id" = "id")) |>
  select(-unnest_referenced_tweets_id)

################################################################################
covid_no_label <- covid_retweets_with_label |>
  filter(is.na(label)) |>
  ungroup() |>
  distinct(unnest_referenced_tweets_id, .keep_all = TRUE)

write.xlsx(covid_no_label, "D:/unlabeled_data.xlsx")
covid_no_label_label <- read.xlsx("D:/unlabeled_data.xlsx")

covid_no_label <- covid_retweets_with_label |>
  filter(is.na(label)) |>
  ungroup() |>
  select(-label)

covid_no_label <- covid_no_label |>
  left_join(covid_no_label_label[, c("unnest_referenced_tweets_id", "label")], 
            by = c("unnest_referenced_tweets_id" = "unnest_referenced_tweets_id"))
  
covid_retweets_with_label <- covid_retweets_with_label |>
  left_join(covid_no_label[, c("id", "label")],
            by = "id") |>
  mutate(label = coalesce(label.x, label.y)) |>
  select(-c(label.x, label.y))

################################################################################

covid_merged <- covid_retweets_with_label |>
  rbind(nort_98_combined) |>
  arrange(created_at)

saveRDS(covid_merged, "D:/Data/Datasets/Classification_data_filtered/covid_classified_without_rt_98_FINAL.RDS")
