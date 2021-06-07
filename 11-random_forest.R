library(tidyverse)
library(tictoc)
library(tidymodels)

## shorter version of the regressions.R script focused on random forest only

set.seed(123415)

# load data:
 load("Results/regression_data_GPP.RData")
#load("Results/regression_data_TER.RData")
# load("Results/regression_data_chlorA.RData")
### Explorations from draft analysis -- to clean up ###


#### Data exploration to decide on variables ####
deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
    mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
    mutate(detected = as_factor(detected),
           detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
    mutate(biome = fct_explicit_na(biome))



## Data split
data_split <- initial_split(
    deltas %>% filter(n_ews == 0 | n_ews >1), #, biome == "Boreal Forests/Taiga"
    strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)


#### Random forest ####
# Random forest does not need a lot of pre-processing and allow variables in their natural units
rf_rcp <-  recipe(
    detected ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 +
        burn_area + biome ,
    data = train_data) %>%
    step_filter(biome != "Rock and Ice") %>%
    #step_log(burn_area, signed = TRUE) %>% 
    #step_rm(contains("var")) %>%
    step_dummy(biome) %>%
    #step_BoxCox(prop_change) %>%  #, starts_with("lcc")
    #step_zv(all_predictors()) %>%
    #step_normalize(all_numeric()) %>%
    #step_interact(terms = ~ prec_var:temp_var) %>%
    themis::step_downsample(detected)

rf_prep <- prep(rf_rcp)
juiced <- juice(rf_prep)

## model specification
rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
    set_mode("classification") %>%
    set_engine("ranger") #importance = "impurity"

## workflow:
rf_workflow <- workflow() %>%
    add_recipe(rf_rcp) %>%
    add_model(rf_model) 


## Train hyperparameter  
set.seed(1234)
folds <- vfold_cv(train_data, v=10)

doParallel::registerDoParallel(cores = 8)

set.seed(0987)
tic()
rf_res <- rf_workflow %>%
  tune_grid(resamples = folds,
            grid = 20) 
toc() ## 4.3 hours on 8 cores parallel...
## the rf_res is 271Mb

rf_res %>% 
    collect_metrics() %>% 
    filter(.metric == "roc_auc") %>% 
    select(mean, min_n, mtry) %>% 
    pivot_longer(cols = min_n:mtry, names_to = "tune_parm", values_to = "value") %>% 
    ggplot(aes(value, mean)) + 
    geom_point(aes(color = tune_parm)) +
    facet_wrap(~tune_parm, scales = "free_x")


rf_grid <- grid_regular(
    mtry(range = c(2,12)), # maybe go up to 12
    min_n(range = c(10,30)),
    levels = 5
)

set.seed(6789)
tic()
regular_res <- tune_grid(
    rf_workflow,
    resamples = folds,
    grid = rf_grid
)
toc() # 1.45hrs

regular_res %>% 
    collect_metrics() %>% 
    filter(.metric == "roc_auc") %>% 
    mutate(min_n = factor(min_n)) %>% 
    ggplot(aes(mtry, mean, color = min_n)) +
    geom_line() +
    geom_point()

best_mod <- select_best(regular_res)

rf_final <- finalize_model(
    rf_model,
    best_mod
)

tic()
rf_final %>%
    set_engine("ranger", importance = "permutation") %>%
    fit(detected ~ .,
        data = juice(rf_prep)) %>%
    vip::vip(geom = "point")
toc()

final_wf <- workflow() %>% 
    add_recipe(rf_rcp) %>% 
    add_model(rf_final)

## train the final on training set and evaluates on test set
final_fit <- final_wf %>% 
    last_fit(data_split)

final_fit$.metrics
## the model does not overfit, same auc on test than on train data

final_fit %>% 
    collect_predictions() %>% 
    mutate(correct = case_when(
        detected == .pred_class ~ "correct", TRUE ~ "incorrect"
    )) %>% 
    bind_cols(test_data) %>% 
    ggplot(aes(lon, lat, fill  = correct)) +
    geom_tile()

## the model is good better at catching up true FALSE values, but does not great 
## with TRUE values. I don't quite get why the sample is so big for undetected values 
## if part of the receip was downsampling on detected.
final_fit %>% 
    collect_predictions() %>% 
    conf_mat(truth = detected, estimate = .pred_class)

save(final_fit, regular_res, rf_res, 
     file = "Results/210529_rf_GPP.RData")



# tic()
# rf_fit <- fit(rf_workflow, train_data)
# toc() # 57s
# 
# 
# tic()
# rf_res <- rf_workflow %>%
#     fit_resamples(
#         resamples = folds,
#         control = keep_pred)
# toc() # 621.442secs
# 
# collect_metrics(rf_res)


# accuracy 0.78, with lcc 81
# roc_auc 0.86, with lcc 88
# accuracy 0.794, roc 0.876 with fire

imp_df <- vip::vi(
    rf_fit$fit$fit, method = "firm", ice = TRUE, 
    train = rf_fit$pre$mold$predictors)

imp_df %>%
    ggplot(aes(Variable, Importance)) +
    geom_col() +
    coord_flip()
#
# rf_res$.predictions %>% bind_rows() %>%
#     ggplot(aes(.pred, abs_delta_std)) +
#     geom_point(size = .2, alpha = .3) +
#     geom_abline() +
#     coord_obs_pred()
