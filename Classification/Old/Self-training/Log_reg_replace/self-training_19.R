## Self-training

# Iteration: 19
# Algorithm: Logistic regression
# Word embeddings dimensions: 100
# Class weights

packages <- c("tidyverse", "tidymodels", "readxl", "ROSE", "recipes", "textrecipes",
              "e1071", "caret", "glmnet", "spacyr", "textdata", "xgboost", "data.table",
              "foreach", "doParallel")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package,
                     dependencies = TRUE,
                     repos='http://cran.us.r-project.org')
  }
}

for (package in packages){
  library(package, character.only = TRUE)
}

spacy_initialize(model = "nb_core_news_lg")
no_we <- fread("~/INORK_ST/model_1.txt", 
               skip = 1, header = FALSE, sep = " ", quote = "", encoding = "UTF-8")

no_we <- no_we |>
  as_tibble()

################################################################################
covid <- readRDS("E:/Data/Training samples/st_log_reg/misinformation_class_18.RDS")

stopwords <- read_xlsx("~/INORK_ST/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

covid$label <- as.factor(covid$label)

set.seed(1234)
covid_split <- initial_split(covid, prop = 0.8, strata = label)
train <- training(covid_split)
test <- testing(covid_split)

train |>
  count(label) # misinfo = 22929, non misinfo = 256430

################################################################################
279359/(table(train$label)[1] * 2) # 6.091827                     
279359/(table(train$label)[2] * 2) # 0.5447081                   

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 6.091827, 0.5447081),
         case_wts = importance_weights(case_wts))

################################################################################
## Logistic regression
set.seed(166)
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

set.seed(1627)
grid <- tibble(penalty = 10^seq(-3, 0, length.out = 10))

# n_cores <- detectCores()
# cluster <- makePSOCKcluster(20)
# cluster <- makeForkCluster(5, outfile = "")
# registerDoParallel(cluster)
# getDoParWorkers()

set.seed(396)
lr_res <- lr_workf |>
  tune_grid(resamples = folds, grid = grid, metrics = cls_metric)

lr_params <- select_best(lr_res, metric = "f_meas")

set.seed(3475)
lr_final_workf <- lr_workf |>
  finalize_workflow(lr_params)

set.seed(26)
lr_final_fit <- fit(lr_final_workf, train)

lr_preds <- test |>
  bind_cols(predict(lr_final_fit, test))

cm_lr <- confusionMatrix(table(test$label, lr_preds$.pred_class)) 
cm_lr$byClass["F1"] # 0.9911734                  
cm_lr$byClass["Precision"] # 0.9991277                   
cm_lr$byClass["Recall"] # 0.9833448               

lr_preds |>
  conf_mat(truth = label, estimate = .pred_class) |> 
  autoplot(type = "heatmap") 

# saveRDS(lr_final_fit, "E:/Data/Training samples/st_log_reg/Log_reg_classifier/round_18.RDS")
################################################################################
covid_df <- readRDS("E:/Data/covid_processed.RDS")
match <- subset(covid, (covid$id %in% covid_df$id))
covid_df <- covid_df |>
  #  anti_join(match, by = "id") |>
  rename(tweet = text)

set.seed(89)
lr_preds_all <- covid_df |>
  bind_cols(predict(lr_final_fit, covid_df, type = "prob"))
# stopCluster(cluster)

lr_preds_all_filtered <- lr_preds_all |>
  filter(.pred_misinfo > 0.99 | .pred_non.misinfo > 0.99)

lr_preds_all_filtered_label <- lr_preds_all_filtered |>
  mutate(label = case_when(
    .pred_misinfo > 0.99 ~ "misinfo",
    .pred_non.misinfo > 0.99 ~ "non.misinfo"
  ))

lr_preds_all_filtered_label <- lr_preds_all_filtered_label |>
  select(tweet, label, id)

lr_preds_all_filtered_label |>
  count(label) # misinfo = 27007, nonmisinfo = 312608

covid_predicted <- full_join(lr_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |>
  count(label) # misinfo = 28791, nonmisinfo = 322730

saveRDS(covid_predicted, "E:/Data/Training samples/st_log_reg/misinformation_class_19.RDS")
