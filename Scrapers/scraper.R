library(tidyverse)
library(httr)
library(jsonlite)

# Defining the parameters for the search

bearer_token = "" # Your bearer token goes here

next_token <- NULL

tweets_search_all <- data.frame()
media <- data.frame()

headers = c(`Authorization` = sprintf('Bearer %s', bearer_token))

params = list(
  `query` = '(korona OR covid OR covid-19 OR corona OR pandemic OR pandemi OR koronavirus OR coronavirus OR SARS-CoV-2) place_country:NO lang:no ', #Search words for the query
  `max_results` = '100',
  `start_time` = '2010-01-01T00:00:00Z',
  `end_time` = '2022-12-31T23:59:00.000Z',
  `media.fields` = 'media_key,type,url,preview_image_url,public_metrics',
  `tweet.fields` = 'author_id,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text',
  `expansions` = 'attachments.media_keys',
  `next_token` = paste0(next_token)
)


response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)

obj <- httr::content(response, as = "text")

obj_json <- fromJSON(obj)

tweets_search_all <- bind_rows(tweets_search_all, obj_json$data)
media <- bind_rows(media, obj_json$includes)

# Creating a loop and consolidating all tweets into a single df
next_token <- obj_json$meta$next_token[1]

while(!is.null(next_token)) {
  
  params = list(
    `query` = '(korona OR covid OR covid-19 OR corona OR pandemic OR pandemi OR koronavirus OR coronavirus OR SARS-CoV-2) place_country:NO lang:no', #Search words for the query
    `max_results` = '100',
    `start_time` = '2020-01-01T00:00:00Z',
    `end_time` = '2022-12-31T23:59:00.000Z',
    `media.fields` = 'media_key,type,url,preview_image_url,public_metrics',
    `tweet.fields` = 'author_id,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text',
    `expansions` = 'attachments.media_keys',
    `next_token` = paste0(next_token)
  )
  
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)
  
  obj <- httr::content(response, as = "text")
  
  obj_json <- fromJSON(obj)
  
  tweets_search_all <- bind_rows(tweets_search_all, obj_json$data)
  media <- bind_rows(media, obj_json$includes)
  
  next_token <- obj_json$meta$next_token[1]
  
  Sys.sleep(3)
  
}


write_rds(tweets_search_all, "D:/Data/covid_no_media_no.rds")
write_rds(media, "D:/Data/covid_media_no.rds")


# tweets <- bind_rows(tweets_search_all, norwegian)
# write_rds(tweets, "D:/Data/norwegian.rds")



# Creating a separate df without any retweets
tweets_without_retweets <- tweets_with_retweets %>%
  filter(str_detect(data.text, "^RT", negate = TRUE))


write_rds(tweets_without_retweets, "D:/Data/tweets_without_retweets.rds")
