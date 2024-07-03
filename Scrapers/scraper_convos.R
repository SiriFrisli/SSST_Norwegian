library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)

## Loading in the tweets previously collected with a keyword query


# Creating a new df, only tweets with at least one reply
conversations <- tweets_search_all[tweets_search_all$public_metrics$reply_count != 0, ]

# Create a ist of all the unique conversation IDs
conversation_ids <- conversations$conversation_id
conversation_ids <- unique(conversation_ids)


## loop 
bearer_token = "" # Put your bearer token here

next_token <- NULL

conversations_all <- data.frame()
media <- data.frame()

headers = c(`Authorization` = sprintf('Bearer %s', bearer_token))


for (i in 1:481) {
  
  
  params = list(
    `query` = paste0("lang:no place_country:NO conversation_id:", conversation_ids[i]),
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
  
  conversations_all <- bind_rows(conversations_all, obj_json$data)
  media <- bind_rows(media, obj_json$includes)
  
  next_token <- obj_json$meta$next_token[1]
  
  while (!is.null(next_token)) {
    
    if (!is.null(next_token)) {
      Sys.sleep(1)
      params = list(
        `query` = paste0("lang:no place_country:NO conversation_id:", conversation_ids[i]),
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
      
      next_token <- obj_json$meta$next_token[1]
      
      conversations_all <- bind_rows(conversations_all, obj_json$data)
      
      media <- bind_rows(media, obj_json$includes)
      
      Sys.sleep(3)
      
      
    }
    else {
      
      next_token <- NULL
      
      break}
  }
  
  
  Sys.sleep(1)
  
} 

# Save the files in batches
write_rds(conversations_all, "D:/Data/conversations_vaccines_no_media_no.rds")
write_rds(media, "D:/Data/conversations_vaccines_media_no.rds")



#########################
## Norwegian covid dfs ##
#########################

## Bind all the different dfs
covid_media <- bind_rows(conversations_covid_media_30000, conversations_covid_media_50000, conversations_covid_media_70000)
covid_no_media <- bind_rows(conversations_covid_no_media_30000, conversations_covid_no_media_50000, conversations_covid_no_media_70000)

## Check that the collected conversations covers all the IDs from the original data
test_conv <- subset(covid_no_media, select=conversation_id)
test_orig<- subset(covid_no_media_orig, select=conversation_id)

test_conv <- unique(test_conv)
test_orig <- unique(test_orig)

match <- subset(test_conv, !(test_conv$data.conversation_id %in% test_orig$data.conversation_id))
# Creates a df with only one observation (NA), indicating that all the conversation IDs from the original data has been covered

# Combine the conversations df with the original df
covid_no_media <- bind_rows(covid_no_media, covid_no_media_orig)


covid_media <- covid_media %>% unnest(media)
covid_media_orig <- covid_media_orig %>% unnest(media)
# Need to unnest the media df

covid_media <- bind_rows(covid_media, covid_media_orig)


# Remove rows with only NAs
covid_no_media <- covid_no_media %>%
  drop_na(id)


## Check for duplicates
# New df without duplicates
covid_no_media_dist <- covid_no_media %>%
  distinct(id, .keep_all = TRUE)

# Create a df with only the duplicates, to inspect them
covid_dup <- covid_no_media %>%
  group_by(id) %>%
  mutate(num_rows = sum(n())) %>%
  filter(num_rows > 1)


# Save the finished dfs
write_rds(covid_no_media_dist, "D:/Data/covid_no_media_norwegian.rds")
write_rds(covid_media, "D:/Data/covid_media_norwegian.rds")
