library(tidyverse)
library(httr)
library(urltools)

covid <- readRDS("D:/Data/Datasets/covid_processed_class99_rt.RDS")

# Want to extract the external links and add two new column to the df:
# 1. Column containing the full link
# 2. Column containing only the domain 

# Creating a new df where I remove any observation without any URLs
# Then filter out non-external URLs
covid_links <- covid |>
  drop_na(urls) |>
  unnest(urls) |> 
  select(-c(start, end, images, status, title, description, unwound_url, media_key))

# Creating a new df, filtering out links to twitter, I only want to keep external links
covid_ext <- covid_links %>%
  filter(!str_detect(display_url, "twitter.com"))

# Extracting the domains, want to find any url-shortener present
# covid_ext_dom <- sapply(covid_ext$expanded_url, function (i) {
#   x <- url_parse(i)
#   c(x$domain)
# })
# 
# covid_ext_dom <- data.frame(dom_url=covid_ext_dom,
#                             id=covid_ext$id)

covid_ext_short <- covid_ext |>
  select(id, expanded_url)

pattern <- c("bit.ly", "dlvr.it", "ift.tt", "ow.ly", "t.co", "buff.ly", "goo.gl", 
             "fal.cn", "zpr.io", "hubs.ly", "ms.spr.ly",  "fb.me", "qoo.ly", "is.gd",
             "bddy.me", "zxc.li", "reut.rs", "tmblr.co", "cutt.ly", "rfr.bz", 
             "mol.im", "osf.io", "who.int", "tiny.cc", "cnn.it", "f24.my", 
             "spoti.fi", "pca.st", "t.me", "dailyclout.io", "hubs.la", "bitchute.xyz",
             "g.co", "m.faz.net")

new_dataframe <- covid_ext |>
  filter(rowSums(sapply(pattern, function(x) grepl(paste0("^",x), display_url))) > 0) |>
  filter(!grepl("^timesofindia", display_url)) |>
  filter(!grepl("^reuters", display_url)) |>
  filter(!grepl("^time", display_url))

httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
expanded_urls <- new_dataframe$expanded_url
num_urls <- length(expanded_urls)
out_df <- data.frame(url = rep(NA, num_urls))

for (i in 1:num_urls) {
  tryCatch({
    # Attempt to get the HEAD response for each URL
    response <- HEAD(expanded_urls[i], timeout(10))  # Set timeout to 10 seconds
    
    # Check the status code
    if (response$status_code == 200) {
      # Store the long URL in the result data frame
      out_df$url[i] <- response$url
    } else {
      # Store 'invalid url' if status code is not 200
      out_df$url[i] <- "invalid url"
    }
  }, error = function(e) {
    # Store 'invalid url' in case of an error
    out_df$url[i] <- "invalid url"
    # Optional: log the error for troubleshooting
    cat("Error with URL ", expanded_urls[i], ": ", e$message, "\n")
  })
}

combined <- new_dataframe |>
  cbind(out_df)

saveRDS(combined, "E:/Data/Datasets/short_urls.RDS")

# Add to the bigger dataset
# Unnesting the list column first
covid_unnested <- covid |>
  unnest(urls, keep_empty = TRUE) |>
  select(-c(start, end, images, status, display_url, title, description, unwound_url, media_key, url))

covid_unnested_wide <- covid_unnested |>
  group_by(id, tweet) |>
  mutate(url_num = paste0("url_", row_number())) |>
  pivot_wider(names_from = url_num, values_from = expanded_url)

covid_unnested_wide <- covid_unnested_wide |>
  ungroup()

# Need to rename a column first
names(combined)[length(names(combined))]<-"url_1" 
combined_short <- combined |>
  select(-c(url, display_url, expanded_url))
combined_short <- combined_short |>
  rename("expanded_url" = "url_1")

combined_short_wide <- combined_short |>
  group_by(id, tweet) |>
  mutate(url_num = paste0("url_", row_number())) |>
  pivot_wider(names_from = url_num, values_from = expanded_url)

# covid_merged <- full_join(combined_short_wide, covid_unnested_wide, by = "id", suffix = c("_A", "_B")) |>
#   mutate(across(ends_with("_A"), ~ coalesce(.x, get(sub("_A", "_B", cur_column()))))) |>
#   select(-ends_with("_B")) |>
#   rename_with(~ sub("_A", "", .x))

# covid_fin <- right_join(combined_short_wide, covid_unnested_wide, by = c("id")) |>
#   mutate(url_1 = coalesce(url_1.x, url_1.y),
#          url_2 = coalesce(url_2.x, url_2.y),
#          url_3 = coalesce(url_3.x, url_3.y),
#          url_4 = coalesce(url_4.x, url_4.y),
#          tweet = coalesce(tweet.y, tweet.x)) |>
#   select(-ends_with(".x")) |>
#   select(-c(url_1.x, url_2.x, url_3.x, url_4.x))
#   rename_with(~ sub(".x", "", .x))

cvoid_merged <- full_join(combined_short_wide, covid_unnested_wide, by = "id", suffix = c("_A", "_B")) |>
  mutate(across(ends_with("_A"), ~ coalesce(.x, get(sub("_A", "_B", cur_column()))))) |>
  select(-ends_with("_B")) |>
  rename_with(~ sub("_A", "", .x))
