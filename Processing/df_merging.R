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
covid_merged <- covid_all_retweets %>%
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
