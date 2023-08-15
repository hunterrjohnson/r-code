# XGBoost using tidymodels

# Description:

set.seed(123)

# Set working directory
wd <- gsub('R/.+$', '', getwd())
setwd(wd)

# Packages
library(tidyverse)
library(tidymodels)
library(workflows)
library(tune)
library(doParallel)
library(tictoc)
library(dplyr)
library(data.table)

#===============================================================================
# Relevant paths / directories

import_paths = list(
  example = "example.csv"
)

export_paths = list()

#===============================================================================
# Import data

# Read data
dat = fread(import_paths$example) %>% data.frame()

# Specify y and x variables for models
y_cols <- c('')
x_cols <- c('')

dat <- dat[, c(y_cols, x_cols) ]

# Filter out missing data
dat = dat %>% filter(if_all(all_of(y_cols), complete.cases))
dat = dat %>% filter(if_all(all_of(x_cols), complete.cases))

# Convert dependent variable to factor
dat = dat %>% 
  mutate(y_var = as_factor(y_var))

#===============================================================================
# Model

# Initial split
dat_split <- initial_split(dat, prop = .75)
dat_train <- training(dat_split)
dat_test  <- testing(dat_split)

table(dat_train$ESA, useNA = 'ifany') %>% prop.table()
table(dat_test$ESA, useNA = 'ifany') %>% prop.table()

# Random forest
model_rf = 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# Grid search parameters
grid_rf =
  grid_max_entropy(        
    mtry(range = c(1, length(x_cols))), 
    trees(range = c(500, 500)),
    min_n(range = c(2, length(x_cols))),
    size = 10) 

# ?grid_max_entropy

# Workflow (NOTE: CONFLICT WITH add_formula)
wkfl_rf =
  workflow() %>% 
  add_formula(y_var ~ .) %>% 
  workflows::add_model(model_rf)

# Cross validation method
cv_folds <- vfold_cv(dat_train, v = 5)
cv_folds

# Choose metrics
my_metrics <- metric_set(roc_auc, accuracy, sens, spec, precision, recall)

# Enable parallel processing
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# Tuning
tic()
rf_fit <- tune_grid(
  wkfl_rf,
  resamples = cv_folds,
  grid = grid_rf,
  metrics = my_metrics,
  control = control_grid(verbose = TRUE,
                         parallel_over = 'resamples')
)
toc()

# Inspect tuning 
rf_fit
collect_metrics(rf_fit)
autoplot(rf_fit, metric = "accuracy")
show_best(rf_fit, metric = "accuracy", maximize = TRUE)
select_best(rf_fit, metric = "accuracy", maximize = TRUE)

# Predict model
tuned_model <-
  wkfl_rf %>% 
  finalize_workflow(select_best(rf_fit, metric = "accuracy", maximize = TRUE)) %>% 
  fit(data = dat_train)

predict(tuned_model, dat_train)
predict(tuned_model, dat_test)
dat = cbind(dat, predict(tuned_model, dat))

table(dat$y_var) %>% prop.table()
table(dat$.pred_class) %>% prop.table()



