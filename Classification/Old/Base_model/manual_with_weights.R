## Base classifier tests
# Manually labeled sample with weights
# SVM, Logistic Regression, Gradient Boosted Trees
# Word embeddings, 100 dimensions

################################################################################
packages <- c("tidyverse", "tidymodels", "readxl", "ROSE", "recipes", "textrecipes",
              "e1071", "caret", "glmnet", "spacyr", "textdata", "xgboost", "data.table")

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

spacy_initialize(model = "nb_core_news_lg") # Spacy environment for lemmas

# Pre-trained word embeddings:
no_we <- fread("~/INORK/model_1.txt", 
               skip = 1, header = FALSE, sep = " ", quote = "", encoding = "UTF-8")
no_we <- no_we |>
  as_tibble()

################################################################################
covid <- read_xlsx("~/INORK/Data/misinformation_labeled_finished.xlsx")
# this data has already been preprocessed some

stopwords <- read_xlsx("~/INORK/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

covid <- covid |>
  select(tweet = text, label, id)

covid$label <- case_when(
  covid$label == 0 ~ "non.misinfo",
  covid$label == 1 ~ "misinfo"
)
covid$label <- as.factor(covid$label)

set.seed(1234)
covid_split <- initial_split(covid, prop = 0.8, strata = label)
train <- training(covid_split)
test <- testing(covid_split)

train |>
  count(label) # misinfo = 64, non misinfo = 799

# calculating the weights
863/(table(train$label)[1] * 2) # 6.742188  
863/(table(train$label)[2] * 2) # 0.5400501 

################################################################################
## SVM

# creating the recipe
covid_recipe <- recipe(label~tweet, data = train) |>
  step_tokenize(tweet, engine = "spacyr") |>
  step_stopwords(tweet, language = "no", keep = FALSE, 
                 stopword_source = "snowball", 
                 custom_stopword_source = custom_words) |>
  step_lemma(tweet) |>
  step_word_embeddings(tweet, embeddings = no_we) |>
  step_normalize(all_numeric_predictors()) |>
  prep()

new_train <- bake(covid_recipe, new_data = train)
new_test <- bake(covid_recipe, new_data = test)

svm_tune <- tune(svm, 
                 label~., 
                 data = new_train, 
                 kernel="radial", 
                 ranges = list(gamma=c(0, 0.001, 0.01, 0.1, 1), 
                               cost = c(0.1, 1, 5, 10)),
                 class.weights = c("misinfo" = 6.742188, "non.misinfo" = 0.5400501),
                 probability = TRUE,
                 tunecontrol = tune.control(sampling = "cross", 
                                            cross = 5, 
                                            sampling.aggregate = mean, 
                                            best.model = TRUE))
svm_tune$best.parameters$gamma # 1
svm_tune$best.parameters$cost # 1

set.seed(1234)
svm_model <- svm(formula = label~.,
                 data = new_train,
                 type = "C-classification",
                 kernel = "radial",
                 cross = 5,
                 class.weights = c("misinfo" = 6.742188, "non.misinfo" = 0.5400501),
                 probability = TRUE)

model_svm <- new_test |>
  bind_cols(predict(svm_model, new_test))

cm_svm <- confusionMatrix(table(new_test$label, model_svm$...102), positive = "misinfo") 
cm_svm$byClass["F1"] # 0.2702703        
cm_svm$byClass["Precision"] # 0.3846154    
cm_svm$byClass["Recall"] # 0.2083333  

new_test |>
  bind_cols(predict(svm_model, new_test)) |>
  conf_mat(truth = label, estimate = ...102) |>
  autoplot(type = "heatmap")

################################################################################
## Logistic regression

# Creating the weights and recipe (same will be used for GBDT and RF)
set.seed(8008)
folds <- vfold_cv(train, strata = label, v = 5, repeats = 2)
cls_metric <- metric_set(yardstick::precision, yardstick::recall, yardstick::f_meas)

train <- train |>
  mutate(case_wts = ifelse(label == "misinfo", 6.742188, 0.5400501),
         case_wts = importance_weights(case_wts))

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
cm_lr$byClass["F1"] # 0.2857143     
cm_lr$byClass["Precision"] # 0.6923077   
cm_lr$byClass["Recall"] # 0.18  

lr_preds |>
  conf_mat(truth = label, estimate = .pred_class) |> 
  autoplot(type = "heatmap") 

################################################################################
## Gradient Boosted Trees
bt_spec <- boost_tree(trees = 500, min_n = tune(), tree_depth = tune(), loss_reduction = tune(), sample_size = tune(), mtry = tune()) |>
  set_mode("classification") |>
  set_engine("xgboost") 

bt_workf <- workflow() |>
  add_model(bt_spec) |>
  add_recipe(model_recipe) |>
  add_case_weights(case_wts)

set.seed(89)
grid <- grid_latin_hypercube(tree_depth(), 
                             min_n(), 
                             loss_reduction(), 
                             sample_size = sample_prop(), 
                             finalize(mtry(), train), 
                             size = 20)

set.seed(1)
bt_res <- bt_workf |>
  tune_grid(resamples = folds, grid = grid, control = control_grid(save_pred = TRUE), metrics = cls_metric)

bt_params <- select_best(bt_res, metric = "f_meas")

set.seed(456)
bt_final_workf <- bt_workf |>
  finalize_workflow(bt_params)

set.seed(2)
bt_final_fit <- fit(bt_final_workf, train)

bt_preds <- test |>
  bind_cols(predict(bt_final_fit, test))

cm_bt <- confusionMatrix(table(test$label, bt_preds$.pred_class)) 
cm_bt$byClass["F1"] # 0.2666667     
cm_bt$byClass["Precision"] # 0.4615385  
cm_bt$byClass["Recall"] # 0.1875     

bt_preds |>
  conf_mat(truth = label, estimate = .pred_class) |>
  autoplot(type = "heatmap") 

################################################################################
## Random forest
rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) |>
  set_mode("classification") |>
  set_engine("ranger")

rf_workf <- workflow() |>
  add_model(rf_spec) |>
  add_recipe(model_recipe) |>
  add_case_weights(case_wts)

set.seed(89)
grid <- grid_latin_hypercube(min_n(), 
                             finalize(mtry(), train), 
                             size = 20)

set.seed(1)
rf_res <- rf_workf |>
  tune_grid(resamples = folds, grid = grid, control = control_grid(save_pred = TRUE), metrics = cls_metric)

rf_params <- select_best(rf_res, metric = "f_meas")

set.seed(456)
rf_final_workf <- rf_workf |>
  finalize_workflow(rf_params)

set.seed(2)
rf_final_fit <- fit(rf_final_workf, train)

rf_preds <- test |>
  bind_cols(predict(rf_final_fit, test))

cm_rf <- confusionMatrix(table(test$label, rf_preds$.pred_class)) 
cm_rf$byClass["F1"] # 0.125 
cm_rf$byClass["Precision"] # 0.07692308 
cm_rf$byClass["Recall"] # 0.3333333 

rf_preds |>
  conf_mat(truth = label, estimate = .pred_class) |>
  autoplot(type = "heatmap") 
