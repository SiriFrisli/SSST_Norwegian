library(tidyverse)
library(httr)
library(urltools)

covid <- readRDS("E:/Data/Datasets/covid_processed_class99_rt.RDS")

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
  select(-c(start, end, images, status, display_url, title, description, unwound_url, media_key))
