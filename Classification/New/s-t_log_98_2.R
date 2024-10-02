## Self-training

# Iteration: 2
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

Sys.setenv(SPACY_PYTHON = "/fp/homes01/u01/ec-sirif/.virtualenvs/r-reticulate")
reticulate::use_virtualenv(Sys.getenv("SPACY_PYTHON", unset = "r-spacyr"))
spacy_initialize(model = "nb_core_news_lg") # Spacy environment for lemmas
no_we <- fread("~/INORK/model_1.txt", 
               skip = 1, header = FALSE, sep = " ", quote = "", encoding = "UTF-8")

no_we <- no_we |>
  as_tibble()

################################################################################
covid <- readRDS("E:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_1_98_nort.RDS")

stopwords <- read_xlsx("~/INORK/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

covid$label <- as.factor(covid$label)

set.seed(1234)
covid_split <- initial_split(covid, prop = 0.8, strata = label)
train <- training(covid_split)
test <- testing(covid_split)

train |>
  ungroup() |>
  count(label) # misinfo = 228, non misinfo = 873

################################################################################
1101/(table(train$label)[1] * 2) # 2.414474      
1101/(table(train$label)[2] * 2) # 0.6305842      

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 2.414474, 0.6305842),
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
cm_lr$byClass["F1"] # 0.625          
cm_lr$byClass["Precision"] # 0.7894737        
cm_lr$byClass["Recall"] # 0.5172414       

lr_preds |>
  conf_mat(truth = label, estimate = .pred_class) |> 
  autoplot(type = "heatmap") 

# saveRDS(lr_final_fit, "~/INORK/NEW/Self_train/Log_reg/Classifier/round_1_98.RDS") # forgot number
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
  count(label) # misinfo = 0, nonmisinfo = 14320

# lr_preds_all_filtered_label |> # 99
#   ungroup() |>
#   count(label) # misinfo = 2308, nonmisinfo = 51821

covid_predicted <- full_join(lr_preds_all_filtered_label, covid, by = "id") |>
  mutate(label = coalesce(label.x, label.y),
         tweet = coalesce(tweet.x, tweet.y)) |>
  select(tweet, label, id)

covid_predicted |> # 98
  ungroup() |>
  count(label) # misinfo = 285, nonmisinfo = 15412

saveRDS(covid_predicted, "E:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_2_98_nort.RDS")
# saveRDS(lr_preds_all, "~/INORK/NEW/Self_train/Log_reg/Results/misinformation_preds_2_99.RDS")


# round_2_check <- readRDS("E:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_2_98_nort.RDS")
# round_1 <- readRDS("E:/Data/Training samples/st_log_reg_98_filtered_nort/misinformation_class_1_98_nort.RDS")
# 
# round_2_check <- round_2_check |>
#   anti_join(round_1, by = "id") |>
#   filter(label == "misinfo")
