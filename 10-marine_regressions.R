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
        upper = list(continuous = GGally::wrap(GGally::ggally_cor, 
                                               display_grid = FALSE, size = 1.5)),
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
        sss_slope +  sss_mean + biome + lon + lat,
    data = train_data) %>%
    step_dummy(biome) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    step_interact(terms = ~ sss_mean:temp_mean) %>%
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
toc() ## 6 hours on 8 cores parallel...
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
  min_n(range = c(2,20)),
  levels = 5
)

set.seed(6789)
tic()
regular_res <- tune_grid(
  rf_workflow,
  resamples = folds,
  grid = rf_grid
)
toc() # 5.2hrs


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
df_plot <- rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(detected ~ .,
      data = juice(rf_prep)) 
toc() #4.2m

df_plot %>% vip::vip(geom = "point", include = TRUE) +
  labs(title = "Random forest on detection", 
       subtitle = "ROC area under the curve 0.829") +
  theme_light(base_size = 8)

ggsave(filename = "figures/fig_regression_cholrA.png", 
       device = "png", dpi = 300,
       width = 3.5, height = 3, units = "in")

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
     file = "Results/210608_rf_chlorA.RData")

#### Partial least squares ####

pls_rcp <- recipe(
  detected ~ temp_std_longterm + temp_std_annual_cycle + 
    temp_std_fast_oscillations + temp_slope + temp_mean +
    sss_std_longterm + sss_std_annual_cycle + sss_std_fast_oscillations +  
    sss_slope +  sss_mean + biome + lon + lat,
  data = train_data) %>%
  step_dummy(biome) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>%
  step_interact(terms = ~ sss_mean:temp_mean) %>%
  themis::step_downsample(detected)

pls_model <- plsmod::pls(num_comp = 4, predictor_prop = 0.8) %>%
  set_engine("mixOmics") %>% 
  set_mode("classification")

pls_workflow <- workflow() %>%
  add_model(pls_model) %>%
  add_recipe(pls_rcp)

tic()
pls_fit <- fit(pls_workflow, train_data)
toc()

pls_fit$fit$fit$fit$loadings$X %>% 
  as_tibble() %>% 
  add_column(., var = rownames(pls_fit$fit$fit$fit$loadings$X)) %>% 
  pivot_longer(cols = 1:2, names_to = "comp", values_to = "value") %>% 
  ggplot(aes(value, var)) +
  geom_col() + 
  facet_wrap(~comp)

tic()
pls_res <- pls_workflow %>%
  fit_resamples(
    resamples = folds,
    control = keep_pred)
toc() # 798s

collect_metrics(pls_res)
## auc 0.58... no better than a coin toss
pls_fit$fit$fit$fit$Y
  
vi_pls <- caret::filterVarImp(x = as.data.frame(pls_fit$fit$fit$fit$X), 
              y = pls_fit$fit$fit$fit$Y,
              nonpara = FALSE)

vi_pls %>% 
  rownames_to_column("term") %>% 
  pivot_longer(cols = 2:3, names_to = "pred", values_to = "importance") %>% 
  ggplot(aes(term, importance)) +
  geom_col(aes(fill = pred), position = "dodge", alpha = 0.4) +
  coord_flip()



#### other figures ####
g1 <- reg_df %>% 
  mutate(response = "Chlorofyll A") %>%
  mutate(p_value = ifelse(p.value > 0.05, "p > 0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%
  mutate(p_value = as_factor(p_value)
         # p_value = fct_relevel(p_value, 
         #                       levels = c("p<0.01", "p<0.05", "p > 0.05")) 
         ) %>% 
  mutate(type = case_when(
    str_detect(term, "temp_|sss_") ~ "Climate",
    str_detect(term, "biome") ~ "Biomes",
    str_detect(term, "Intercept|lon|lat") ~ ".")) %>% 
  mutate(term = str_remove_all(term, "biome_")) %>% 
  ggplot(aes(estimate, term)) +
  geom_point(aes(color = p_value), size = 0.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, color = p_value), size = 0.25) +
  geom_vline(xintercept = 1, size = 0.3) + 
  facet_grid(type~response, scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual("", values = c("#6a3d9a", "#ff7f00", "#a6cee3")) +
  theme_light(base_size = 6) +
  theme(legend.position = "top")

g1 + theme_light(base_size = 10)

ggsave(filename = "figures/fig_log_regression_ClorA.png", plot = g1,
       device = "png", dpi = 300,
       width = 4, height = 5, units = "in")
