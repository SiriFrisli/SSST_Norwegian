## Self-training

# Iteration: 18
# Algorithm: Logistic regression
# Word embeddings dimensions: 100
# Class weights

packages <- c("tidyverse", "tidymodels", "readxl", "ROSE", "recipes", "textrecipes",
              "e1071", "caret", "glmnet", "spacyr", "textdata", "xgboost", "data.table")

# for (package in packages) {
#   if (!require(package, character.only = TRUE)) {
#     install.packages(package,
#                      dependencies = TRUE,
#                      repos='http://cran.us.r-project.org')
#   }
# }

for (package in packages){
  library(package, character.only = TRUE)
}

spacy_initialize(model = "nb_core_news_lg")
no_we <- fread("~/INORK_ST/model_1.txt", 
               skip = 1, header = FALSE, sep = " ", quote = "", encoding = "UTF-8")

no_we <- no_we |>
  as_tibble()

################################################################################
covid <- readRDS("E:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_17_98_nort.RDS")

stopwords <- read_xlsx("~/INORK_st/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

covid$label <- as.factor(covid$label)

set.seed(1234)
covid_split <- initial_split(covid, prop = 0.8, strata = label)
train <- training(covid_split)
test <- testing(covid_split)

train |>
  ungroup() |>
  count(label) # misinfo = 12249, non misinfo = 150240

################################################################################
162489/(table(train$label)[1] * 2) # 6.632746                  
162489/(table(train$label)[2] * 2) # 0.5407648                

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 6.632746, 0.5407648),
         case_wts = importance_weights(case_wts))

################################################################################
## Logistic regression
set.seed(8008)
folds <- vfold_cv(train, strata = label, v = 5, repeats = 1)
cls_metric <- metric_set(yardstick::precision, yardstick::recall, yardstick::f_meas)

model_recipe <- recipe(label~tweet+case_wts, data = train) |>
  step_tokenize(tweet, engine = "spacyr") |>
  step_stopwords(tweet, language = "no", keep = FALSE, 
                 stopword_source = "snowball", 
                 custom_stopword_source = custom_words) |>
  step_lemma(tweet) |>
  step_word_embeddings(tweet, embeddings = no_we) |>
  step_normalize(all_numeric_predictors())

lr_spec <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_mode("classification") |>
  set_engine("glmnet")

lr_workf <- workflow() |>
  add_model(lr_spec) |>
  add_recipe(model_recipe) |>
  add_case_weights(case_wts)

set.seed(13)
grid <- tibble(penalty = 10^seq(-3, 0, length.out = 20))

set.seed(1)
lr_res <- lr_workf |>
  tune_grid(resamples = folds, grid = grid, metrics = cls_metric)

lr_params <- select_best(lr_res, metric = "f_meas")

set.seed(345)
lr_final_workf <- lr_workf |>
  finalize_workflow(lr_params)

set.seed(2)
lr_final_fit <- fit(lr_final_workf, train)

lr_preds <- test |>
  bind_cols(predict(lr_final_fit, test))

cm_lr <- confusionMatrix(table(test$label, lr_preds$.pred_class)) 
cm_lr$byClass["F1"] # 0.9821833                   
cm_lr$byClass["Precision"] # 0.9973684                  
cm_lr$byClass["Recall"] # 0.9674537                 

# lr_preds |>
#   conf_mat(truth = label, estimate = .pred_class) |> 
#   autoplot(type = "heatmap") 

# saveRDS(lr_final_fit, "~/INORK/NEW/Self_train/Log_reg/Classifier/round_12_98.RDS")
################################################################################
covid_df <- readRDS("E:/Data/Datasets/Classification_data_filtered/covid_relevant_url_nort.RDS")

match <- subset(covid, (covid$id %in% covid_df$id))
covid_df <- covid_df |>
  anti_join(match, by = "id")

set.seed(89)
lr_preds_all <- covid_df |>
  bind_cols(predict(lr_final_fit, covid_df, type = "prob"))

lr_preds_all_filtered <- lr_preds_all |>
  filter(.pred_misinfo...25  > 0.98 | .pred_non.misinfo...26 > 0.98)

lr_preds_all_filtered_label <- lr_preds_all_filtered |>
  mutate(label = case_when(
    .pred_misinfo...25 > 0.98 ~ "misinfo",
    .pred_non.misinfo...26 > 0.98 ~ "non.misinfo"
  ))

lr_preds_all_filtered_label <- lr_preds_all_filtered_label |>
  select(tweet, label, id)

lr_preds_all_filtered_label |> # 98
  ungroup() |>
  count(label) # misinfo = 253, nonmisinfo = 3123

covid_predicted <- full_join(lr_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |> # 98
  ungroup() |>
  count(label) # misinfo = 15542, nonmisinfo = 190946

saveRDS(covid_predicted, "E:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_18_98_nort.RDS")
