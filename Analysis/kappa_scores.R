library(tidyverse)
library(readxl)
library(irr)

covid <- read_xlsx("D:/Data/Training samples/misinformation_labeled.xlsx")

# Creating a df with the obs where the two anotaters disagreed
covid <- covid |>
  select(id, text, label_1, label_2, expanded_url)

covid$label_1 <- as.numeric(covid$label_1)
covid$label_2 <- as.numeric(covid$label_2)

covid_disagree <- covid |>
  filter(label_1 - label_2 != 0)

covid_disagree <- covid_disagree |>
  select(id, text, expanded_url)

write.csv2(covid_disagree, "D:/Data/Training samples/misinformation_labeled_clean_2.csv")

# Creating the final df with at least two annotaters agreeing
covid <- read_xlsx("D:/Data/Training samples/misinformation_labeled.xlsx")

covid <- covid |>
  mutate(label = case_when(
    label_4 == 0 ~ 0,
    label_4 == 1 ~ 0,
    label_4 == 2 ~ 1,
    label_4 == 3 ~ 1
  ))

covid_fin <- covid |>
  select(id, text, label, expanded_url)

write.csv2(covid_fin, "D:/Data/Training samples/misinformation_labeled_finished.csv")


# Calculating the Kappa score
covid <- read_xlsx("D:/Data/Training samples/misinformation_labeled.xlsx")

covid <- covid |>
  select(c(label_1, label_2, label_3))

kappa_result_2 <- kappa2(covid, weight = "unweighted")
kappa_result_2

kappa_result_3 <- kappam.light(covid[,1:3])
kappa_result_3
