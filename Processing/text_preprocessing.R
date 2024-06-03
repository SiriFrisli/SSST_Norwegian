## Preprocessing

library(tidyverse)

# Loading and preprocessing the data, so that thre's no need to do those steps each time the data is loaded
covid <- readRDS("D:/Data/covid_nort_domain.RDS")

removeURL <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "", tweet))
}

covid$text <- apply(covid["text"], 1, removeURL)
covid$text <- apply(covid["text"], 1, removeUsernames)

covid$text <- tolower(covid$text)
covid$text <- gsub("[[:punct:]]", " ", covid$text)

covid <- covid |>
  select(id, text, dom_url, conversation_id, created_at, author_hash, in_reply_to_user_hash)

# saving the processed df
saveRDS(covid, "D:/Data/covid_processed.RDS")

################################################################################
## Fixing the labeled sample
covid_samp <- read_xlsx("D:/Data/Training samples/Old manual label/misinformation_labeled.xlsx")
covid_samp$text <- apply(covid_samp["text"], 1, removeURL)
covid_samp$text <- apply(covid_samp["text"], 1, removeUsernames)
covid_samp$text <- tolower(covid_samp$text)

# Want to only keep the external links within the texts
covid_match <- subset(covid, (covid$id %in% covid_samp$id))

covid_match <- covid_match |>
  drop_na(urls)

covid_match <- covid_match |>
  unnest(urls) |>
  select(-c(start, end, unwound_url))

# Creating a new df, filtering out links to twitter, I only want to keep external links
covid_ext <- covid_match |>
  filter(!str_detect(display_url, "twitter.com")) |>
  filter(!str_detect(display_url, "t.co"))

covid_ext <- covid_ext |>
  select(id, text, conversation_id, expanded_url)

covid_samp$expanded_url <- NA
covid_samp <- covid_samp |>
  select(id, text, conversation_id, expanded_url)


covid_label <- full_join(covid_ext, covid_samp, by = "id") |>
  mutate(expanded_url = coalesce(expanded_url.x, expanded_url.y),
         text = coalesce(text.x, text.y),
         conversation_id = coalesce(conversation_id.x, conversation_id.y)) |>
  select(id, text, conversation_id, label, claim, description, fact_check, expanded_url)

covid_label <- covid_label[!duplicated(covid_label$id), ]

covid_2 <- read_xlsx("D:/Data/Training samples/misinformation_class_clean.xlsx")

covid_2 <- covid_2 |>
  sample_n(28)

covid_label_2 <- full_join(covid_label, covid_2, by = "id") |>
  mutate(text = coalesce(text.x, text.y),
         label = coalesce(label.x, label.y),
         expanded_url = coalesce(expanded_url.x, expanded_url.y)) |>
  select(id, text, label, claim, description, fact_check, expanded_url)

write_csv2(covid_label_2, "D:/Data/Training samples/misinformation_labeled.csv")

covid_label_2 <- covid_label_2[!duplicated(covid_label_2$id), ]
