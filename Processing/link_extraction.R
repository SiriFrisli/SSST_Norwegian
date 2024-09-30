packages <- c("tidyverse", "urltools", "httr", "RCurl")

for (x in packages) {
  if (!require (x, character.only = TRUE)) {
    install.packages(x) 
  } else {
    library(x, character.only = TRUE)
  }
}

sites <- read.csv("https://github.com/JanaLasser/misinformation_domains/raw/main/data/clean/disinformation_domains_clean.csv")

# Filter out sites that were last updated before the pandemic
sites <- sites |> 
  filter(last_update != 2016, last_update != "2016-01-18", last_update != 2017, 
         last_update != "2017-08-08", last_update != "2017-11-09", 
         last_update != 2018, last_update != "2019-01-28")

write.csv(sites, "~/INORK/INORK_R/Processing/misinfo_sites.csv")
# Saving the list, it will be updated manually with Norwegian sites

################################################################################
covid <- readRDS("D:/Data/covid_media_norwegian.rds")

sites <- read.csv2("~/INORK/INORK_R/Processing/misinfo_sites.csv")
sites <- sites |>
  select(c("url", "label", "source", "last_update"))

covid_links <- covid %>%
  drop_na(urls)

covid_unnest <- covid %>%
  unnest(urls) %>%
  select(-c(start, end, unwound_url))

# Creating a new df, filtering out links to twitter, I only want to keep external links
covid_ext <- covid_unnest %>%
  filter(!str_detect(display_url, "twitter.com"))

covid_ext$expanded_url <- str_remove_all(covid_ext$expanded_url, "^https?://")
covid_ext$expanded_url <- str_remove_all(covid_ext$expanded_url, "www.")

################################################################################
# removeURL <- function(tweet) {
#   return(gsub("https?:(//|\\\\)(www\\.)?", "", tweet))
# }
# 
# covid_ext$url <- apply(covid_ext["expanded_url"], 2, removeURL)
# 
# covid_ext$url <- covid_ext$url %>% 
#   tolower()

covid_ext_dom <- sapply(covid_ext$expanded_url, function (i) {
  x <- url_parse(i)
  c(x$domain)
})

covid_ext_dom <- data.frame(dom_url=covid_ext_dom,
                            id=covid_ext$id)

covid_ext_dom <- covid_ext_dom |>
  count(dom_url)


covid_ext_dom$dom_url <- str_remove_all(covid_ext_dom$dom_url, "www.")


covid_ext_dom_count <- covid_ext_dom |>
  count(dom_url)


covid_ext_dom <- covid_ext_dom |>
  rowid_to_column("id2")

covid_ext <- covid_ext |>
  rowid_to_column("id2")

covid_expanded <- covid_ext_dom |>
  merge(covid_ext, by = "id2")

################################################################################
covid_expanded_without <- covid_expanded |>
  anti_join(sites, by = c("dom_url" = "url")) # 111 331 

covid_expanded_with <- covid_expanded |>
  semi_join(sites, by = c("dom_url" = "url")) |>
  select(dom_url, id = id.x, text, conversation_id) # 4403
covid_expanded_with$label <- 1

covid_expanded_with <- covid_expanded_with[!duplicated(covid_expanded_with$id),] # 4319

# Also want the domain name to be added to the text column, so that in can be used as a feature during text classification
covid_expanded_with$text <- paste(covid_expanded_with$text, covid_expanded_with$dom_url, sep = ", ")

# Saving the df with tweets sharing links from low-credibility sources
saveRDS(covid_expanded_with, "D:/Data/covid_misinfo_links_only.RDS")


# For fitlering out these from a previously manually labeled sample
# covid_sample <- read_xlsx("D:/Data/Training samples/misinformation_labeled.xlsx")
# matches <- covid_sample |>
#   inner_join(covid_expanded_with, by = "id")
# 
# covid_sample_2 <- covid_sample |>
#   anti_join(matches, by = "id")
# 
# write.csv2(covid_sample_2, "D:/Data/Training samples/misinformation_labeled.csv")

################################################################################
# Also want the domain name to be added to the text column, so that in can be used as a feature during text classification
covid_expanded$text <- paste(covid_expanded$text, covid_expanded$dom_url, sep = ", ")

covid_expanded <- covid_expanded[!duplicated(covid_expanded$id.x),]

covid_expanded <- covid_expanded |>
  select(id = id.x, text)

covid_test <- covid |>
  left_join(covid_expanded, by = "id")

covid_test <- covid_test |>
  mutate(text.z = coalesce(text.y, text.x))

covid_test <- covid_test |>
  select(-c(text.y, text.x)) |>
  rename(text = text.z)

# saving this df
write_rds(covid_test, "D:/Data/covid_relevant_nort_domain.RDS")


# covid <- read_xlsx("D:/Data/Training samples/misinformation_labeled.xlsx")
# 
# covid <- covid |>
#   left_join(covid_relevant_filtered_domain, by = "id") |>
#   select(id, text.x, conversation_id.x, date.x, label, text.y) |>
#   mutate(text = coalesce(text.y, text.x)) |>
#   select(-c(text.x, text.y))
# 
# write.csv2(covid, "D:/Data/covid_misinfo.csv")

################################################################################
## Need to extract full domain from shortened URLs

covid_ext <- covid_ext |>
  select(id, text, expanded_url)

# Shorteners: bit.ly, dlvr.it, ift.tt, ow.ly, t.co, buff.ly

pattern <- c("bit.ly", "dlvr.it", "ift.tt", "ow.ly", "t.co", "buff.ly")

# covid_ext <- covid_ext |>
#   filter(grepl("bit.ly | dlvr.it | ift.tt | ow.ly | t.co | buff.ly", expanded_url))

covid_ext_filter <- covid_ext |>
  filter(str_detect(expanded_url, paste(pattern, collapse = "|")))

covid_ext_bit <- covid_ext |>
  filter(str_detect(expanded_url, "^bit.ly"))
covid_ext_dlvr <- covid_ext |>
  filter(str_detect(expanded_url, "^dlvr.it"))
covid_ext_ift <- covid_ext |>
  filter(str_detect(expanded_url, "^ift.tt"))
covid_ext_ow <- covid_ext |>
  filter(str_detect(expanded_url, "^ow.ly"))
covid_ext_t <- covid_ext |>
  filter(str_detect(expanded_url, "^t.co"))
covid_ext_buff <- covid_ext |>
  filter(str_detect(expanded_url, "^buff.ly"))

covid_short <- rbind(covid_ext_bit, covid_ext_dlvr, covid_ext_ift, covid_ext_ow, covid_ext_t, covid_ext_buff)

full_url <- sapply(covid_short$expanded_url, function(i) {
  x <- HEAD(i)
})

httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
expanded_urls <- covid_ext_buff$expanded_url
out_df <- data.frame()

for (i in 1:length(expanded_urls)) {
  valid_url <- TRUE
  while(valid_url){
    x <- HEAD(expanded_urls[i])
    out_df <- bind_rows(out_df, data.frame(x$url))
    valid_url <- httr::GET(expanded_urls[i])$status_code == "200"
    if(!valid_url) {
      next
    } else { i <- i+1 }
  }
}
# 
# covid_ext_buff_head <- covid_ext_buff |>
#   head(100)

out_df <- data.frame()
out <- covid_short |>
  mutate(response = map(paste0(out_df, expanded_url), 
                        safely(httr::GET), httr::timeout(3)))

# out <- out_1
# out[,"url"] = NA
# for (i in 1:length(out)) {
#   while(!is.null(out$response[[i]]$result$url == TRUE)){
#     url <- out$response[[i]]$result$url
#     out <- cbind(out, url)
#     if(is.null(out$response[[i]]$result$url == TRUE)) {
#       out$url[i] <- "na"
#     } else { i <- i+1 }
#   }
# }
# 
# 
# for (i in 1:length(out)) {
#   url_long <- out$response[[i]]$result$url
#   if(!is.null(out$response[[i]]$result$url == TRUE)){
#     out[[url_long]] <- out[["url"]]
#   } else {
#     out[["na"]] <- out[["url"]]
#   }
# }
# 
# nested_out <- lapply(out$response, function(x) {
#   x <- x[x != "available"]  # Remove "available" from vector
#   if (length(x) < 1){
#     # Here resulting vector is empty, so return NULL
#     return(NULL)
#   } else {
#     return(x)
#   }
# })


out_unnest <- out |>
  unnest(response)

out_unnest <- out_unnest |>
  rowwise() |>
  mutate(url = list(response[[1]]) ) |>
  ungroup()

out_unnest <- out_unnest |>
  unnest(url, keep_empty = TRUE)

out_unnest <- out_unnest |>
  select(id, text, expanded_url, url)

out_unnest <- out_unnest[!duplicated(out_unnest[c(1,3)]),]


covid_url <- left_join(covid_short, out_unnest, by = c("id", "text", "expanded_url")) 

saveRDS(covid_url, "D:/Data/covid_long_urls.RDS")

################################################################################
## Adding the new long links to the actual dataset

covid_long <- readRDS("D:/Data/covid_long_urls.RDS")
covid <- readRDS("D:/Data/covid_relevant_nort_domain.RDS") # domain name in the dom_url column

covid_long_dom <- sapply(covid_long$url, function (i) {
  x <- url_parse(i)
  c(x$domain)
})

covid_long_dom <- data.frame(dom_url=covid_long_dom,
                             id=covid_long$id)

covid_long_dom$dom_url <- str_remove_all(covid_long_dom$dom_url, "www.")

covid_long_dom_count <- covid_long_dom |>
  count(dom_url)

covid_long_dom <- covid_long_dom |>
  rowid_to_column("id2")

covid_long <- covid_long |>
  rowid_to_column("id2")

covid_expanded <- covid_long_dom |>
  merge(covid_long, by = "id2") |>
  select(text, id = id.x, expanded_url, url, dom_url)

covid_fin <- left_join(covid, covid_expanded, by = c("id")) |>
  mutate(dom_url = coalesce(dom_url.y, dom_url.x),
         text = coalesce(text.x, text.y)) |>
  select(-c(dom_url.x, dom_url.y, text.x, text.y, expanded_url, url))

saveRDS(covid_fin, "D:/Data/covid_nort_domain.RDS")
