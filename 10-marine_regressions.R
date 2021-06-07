library(tidyverse)
library(tictoc)
library(tidymodels)


set.seed(123415)

# load data:
# load("Results/regression_data_GPP.RData")
# load("Results/regression_data_TER.RData")
load("Results/regression_data_chlorA.RData")
#### Data exploration to decide on variables ####
deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
    mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
    mutate(detected = as_factor(detected),
           detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
    mutate(biome = fct_explicit_na(biome))

g <- deltas %>% ungroup() %>%
    select(starts_with("delta"), detected) %>%
    #corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
    GGally::ggpairs(
        aes(fill = detected, color = detected),
        lower = list(continuous = GGally::wrap("points", alpha = 0.15)),
        upper = list(continuous = GGally::wrap(GGally::ggally_cor, display_grid = FALSE, size = 1.5)),
        diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.25)),
        columnLabels = c("Autocorrelation", "Fractal dimension", "Kurtosis", "Skewness", "Standard deviation", "Detected")) +
    scale_color_brewer(palette = "Set1", direction = -1) + 
    scale_fill_brewer(palette = "Set1", direction = -1) +
    theme_light(base_size = 6)

g

ggsave(plot = g, 
       filename = "figures/SM_fig_clorA.png", 
       device = "png", dpi = 300,
       width = 4, height = 3.5, units = "in")

## Note that you can use the absolute value to push all the distribution to positive numbers.
## It seems symmetric without abs.
deltas %>%
    filter(n_ews == 0 | n_ews > 1) %>%
    select(starts_with("delta"), detected) %>%
    pivot_longer(cols = starts_with("delta"), names_to = "stat", values_to = "value") %>%
    ggplot(aes((value))) +
    geom_density(aes(fill = detected), alpha = 0.5) +
    facet_wrap(~stat, scales = "free")

## temperature:
deltas %>% 
  ungroup() %>% 
  select(contains("temp"), detected) %>%
  # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
  GGally::ggpairs(
    aes(fill = detected, color = detected),
    lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

# note that we have missing values for sss variables
deltas %>% ungroup() %>% skimr::skim()

deltas <- deltas %>%
  ungroup() %>% 
  filter(!is.na(sss_slope))

## salinity
# tic()
# deltas %>% 
#     select(contains("sss"), detected, delta_std) %>%
#    GGally::ggpairs(
#       aes(fill = detected, color = detected))
# toc()

## temperature

## get rid of missing values:
deltas <- deltas %>% 
  ungroup() %>% 
  filter(!is.na(sss_slope))

## Data split
data_split <- initial_split(
    deltas %>% filter(n_ews == 0 | n_ews >1), 
    strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)

#### Linear regression:####
lm_rcp <- recipe(
    delta_std ~ temp_mean + temp_slope + temp_var + prec_mean + prec_slope +
        prec_var + biome ,
    data = train_data) %>%
    step_log(burn_area ,signed = TRUE) %>% 
    step_dummy(biome) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())





#### logistic regression ####
log_rcp <- recipe(
    detected ~ temp_std_longterm + temp_std_annual_cycle + 
      temp_std_fast_oscillations + temp_slope + temp_mean +
        sss_std_longterm + sss_std_annual_cycle + sss_std_fast_oscillations +  
        sss_slope +  sss_mean + biome ,
    data = train_data) %>%
    step_dummy(biome) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    #step_interact(terms = ~ prec_var:temp_var) %>%
    themis::step_downsample(detected)

log_model <- logistic_reg() %>%
  set_engine("glm", family = "binomial")

log_workflow <- workflow() %>%
  add_model(log_model) %>%
  add_recipe(log_rcp)

tic()
log_fit <- fit(log_workflow, train_data)
toc() #1.2s

tic()
reg_df <- pull_workflow_fit(log_fit) %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) 
toc() #26s

folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
log_res <- log_workflow %>%
  fit_resamples(
    resamples = folds,
    control = keep_pred)
toc() #10s

collect_metrics(log_res) # roc 0.6

log_fit$fit$fit

log_auc <- log_res %>%
  collect_predictions() %>%
  roc_curve(detected, .pred_TRUE) %>%
  autoplot()








#### random forest ####
rf_rcp <- recipe(
  detected ~ temp_std_longterm + temp_std_annual_cycle + 
    temp_std_fast_oscillations + temp_slope + temp_mean +
    sss_std_longterm + sss_std_annual_cycle + sss_std_fast_oscillations +  
    sss_slope +  sss_mean + biome ,
  data = train_data) %>%
  step_dummy(biome) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>%
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
