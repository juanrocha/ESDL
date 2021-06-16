library(tidymodels)
library(tidyverse)
library(tictoc)

# load data:
load("Results/regression_data_GPP.RData")
# load("Results/regression_data_TER.RData")
# load("Results/regression_data_chlorA.RData")

#### Data exploration to decide on variables ####
deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
    mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
    mutate(detected = as_factor(detected),
           detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
    mutate(biome = fct_explicit_na(biome))

## Data split
set.seed(777)
data_split <- initial_split(
    deltas %>% filter(n_ews == 0 | n_ews >1), #, biome == "Boreal Forests/Taiga"
    strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)
val_set <- validation_split(train_data)

#### Multiple elastic regression ####

ews_rcp <- recipe(
    delta_std ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 +
        burn_area + biome + lat + lon,
    data = train_data) %>%
    step_filter(biome != "Rock and Ice") %>%
    step_log(burn_area, signed = TRUE) %>% 
    #step_rm(contains("var")) %>%
    step_dummy(biome) %>%
    step_BoxCox(prop_change) %>%  #, starts_with("lcc")
    step_normalize(all_numeric()) %>%
    step_zv(all_predictors()) %>%
    step_nzv(all_predictors()) #%>% 
    
    #step_interact(terms = ~ prec_var:temp_var) %>%
    #themis::step_downsample(detected)

lasso_model <- linear_reg( 
    penalty = tune(),
    mixture = tune())  %>%
    set_engine("glmnet") 

lasso_grid <- grid_regular(
    penalty(), mixture(), levels = 5)

lasso_workflow <- workflow() %>%
    add_model(lasso_model) %>%
    add_recipe(ews_rcp)

tic()
lasso_fit <- fit(lasso_workflow, train_data)
toc() #2s

pull_workflow_fit(lasso_fit) %>% broom::tidy()


# pull_workflow_fit(lasso_fit) %>% 
#     broom::tidy() %>% # conf.int = TRUE, exponentiate = TRUE
#     mutate(p_value = ifelse(p.value > 0.05, "p > 0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%
#     ggplot(aes(estimate, term)) +
#     geom_point(aes(shape = as_factor(p_value), color = as_factor(p_value))) +
#     geom_errorbarh(
#         aes(xmin = estimate - std.error, xmax = estimate + std.error, color = as_factor(p_value))) +
#     geom_hline(xintercept = 1) +
#     theme_light()


# folds <- vfold_cv(train_data, v=10)
# keep_pred <- control_resamples(save_pred = TRUE)

tic()
lasso_res <- lasso_workflow %>%
    tune_grid(
        val_set,
        grid = lasso_grid,
        control = control_grid(save_pred = TRUE))
toc()  #23s

lasso_res %>% collect_metrics() %>% 
    ggplot(aes(x = penalty, y = mean)) +
    geom_point() +
    geom_line() +
    ylab("Area under the ROC Curve") +
    scale_x_log10(labels = scales::label_number())


lasso_res$.notes

collect_metrics(lasso_res)
