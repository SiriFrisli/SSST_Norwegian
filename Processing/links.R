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

# Extracting the domains, want to find any url-shortener present
covid_ext_dom <- sapply(covid_ext$expanded_url, function (i) {
  x <- url_parse(i)
  c(x$domain)
  })

covid_ext_dom <- data.frame(dom_url=covid_ext_dom,
                            id=covid_ext$id)

# Creating a new df, filtering out links to twitter, I only want to keep external links
covid_ext <- covid_links %>%
  filter(!str_detect(display_url, "twitter.com"))


covid_ext_short <- covid_ext |>
  select(id, expanded_url)


pattern <- c("bit.ly", "dlvr.it", "ift.tt", "ow.ly", "t.co", "buff.ly", "goo.gl", 
             "fal.cn", "zpr.io", "hubs.ly", "ms.spr.ly",  "fb.me", "qoo.ly", "is.gd",
             "bddy.me", "zxc.li", "reut.rs", "tmblr.co", "cutt.ly", "rfr.bz", 
             "mol.im", "osf.io", "who.int", "tiny.cc", "cnn.it", "f24.my", 
             "spoti.fi", "pca.st", "t.me", "dailyclout.io", "hubs.la", "bitchute.xyz",
             "g.co", "gj.sn", "m.faz.net")

new_dataframe <- covid_ext |>
  filter(rowSums(sapply(pattern, function(x) grepl(paste0("^",x), display_url))) > 0) |>
  filter(!grepl("^timesofindia", display_url)) |>
  filter(!grepl("^reuters", display_url)) |>
  filter(!grepl("^time", display_url))

httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
expanded_urls <- new_dataframe$expanded_url
out_df <- data.frame()

for (i in 1:length(expanded_urls)) {
  valid_url <- TRUE
  while(valid_url){
    x <- HEAD(expanded_urls[i])
    if (x$status_code == 200) {
      out_df <- bind_rows(out_df, data.frame(url = x$url))
      valid_url <- FALSE
    } else {
      out_df <- bind_rows(out_df, data.frame(url = "invalid url"))
      valid_url <- FALSE
    }
  }
}


out_df <- data.frame()
out <- covid_short |>
  mutate(response = map(paste0(out_df, expanded_url), 
                        safely(httr::GET), httr::timeout(3)))