library(tidyverse)
library(tictoc)
library(tidymodels)


set.seed(123415)

# load data:
# load("Results/regression_data_GPP.RData")
# load("Results/regression_data_TER.RData")
 load("Results/regression_data_chlorA.RData")
### Explorations from draft analysis -- to clean up ###


#### Data exploration to decide on variables ####
deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
    mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
    mutate(detected = as_factor(detected),
           detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
    mutate(biome = fct_explicit_na(biome))

# 
# deltas %>%
#     filter(n_ews == 0 | n_ews >1) %>%
#     ggplot(aes(temp_std_fast_oscillations, abs(delta_std))) +
#     geom_point(aes(color = biome), show.legend = FALSE, size = 0.5, alpha = .4) +
#     geom_smooth() +
#     facet_wrap(biome~detected)

## correlation matrix for deltas of EWS
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
    ggplot(aes(abs(value))) +
    geom_density(aes(fill = detected), alpha = 0.5) +
    facet_wrap(~stat, scales = "free")

## For rain, one could drop the slope (very little variability) and the mean_rain (high corr)
deltas %>% select(prop_change, contains("prec"), detected, delta_std) %>%
    # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
    GGally::ggpairs(
        aes(fill = detected, color = detected),
        lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## For temperature, there is a lot of correlation, but the distributions are quite different
## between detected and non-detected. That can be accounted by biome (or lat, lon controls).
## The hypothesis is whether is slow change or stochastic processes, for T I think one should leave them all in.
deltas %>% select(prop_change, contains("temp"), detected) %>%
    # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
    GGally::ggpairs(
        aes(fill = detected, color = detected),
        lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

deltas %>%
    ggplot(aes(temp_slope)) +
    geom_boxplot(aes(fill = detected), alpha = .4) +
    facet_wrap(~biome)

## land use: very little correlations, but the units are all over the place, needs rescaling. The original units are the number of pixels that changed per lc class between 2018 and 1994. Zero if no change, + if increased per class, - if decreased. Does the increase of a class (e.g. agriculture) predicts the EWS?
## On the regression model, probably useful to have abs(delta) instead.
deltas %>%
    select(delta_std, detected, contains("lcc_")) %>%
    select(1,2,21:30) %>%
    # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
    GGally::ggpairs(
        aes(fill = detected, color = detected),
        lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## prop_change has a long-tail distribution, very skewed, needs tranformation. maybe log1p or box-cox
deltas %>% ggplot(aes(prop_change)) +
    geom_density(aes(fill = detected), alpha = .3) +
    scale_x_continuous(trans = "log1p") +
    facet_wrap(~biome, scales = "free")

deltas %>%
    ggplot(aes(delta_ac1, prop_change)) +
    geom_point(aes(color = biome, alpha = detected), show.legend = FALSE) +
    facet_wrap(~biome)

## fires:
deltas %>% ggplot(aes(detected, log1p(burn_area))) + 
  geom_boxplot() +
  facet_wrap(~biome)

## Rock and ice should be dropped, maybe inland water?
deltas %>%
    ggplot(aes(y = abs(delta_std), x = (temp_slope))) +
    geom_point(aes(color = detected), size = 0.5, alpha = 0.6) +
    geom_smooth(aes(group = detected, color = detected)) +
    facet_wrap(~biome, scales = "free") +
    theme_light()

## pull appart the distributions by biome and metric
deltas %>%
    filter(ews_delta_std == TRUE & detected == TRUE | ews_delta_std == FALSE & detected == FALSE) %>%
    ggplot(aes(temp_slope, prec_std_fast_oscillations)) +
    geom_point(aes(color = detected), alpha = .3) +
    #geom_smooth() +
    facet_wrap(~biome)


## J210330: Because delta has a its important values at the tail of the distribution, I probably need to transform it. A good option is the sqrt, it seems to separate trends between detected and undetected on the plot above

##### Regressions ####

## Data split
data_split <- initial_split(
    deltas %>% filter(n_ews == 0 | n_ews >1), #, biome == "Boreal Forests/Taiga"
    strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)

## data-processing and feature engineering:
## land-use terms: lcc_10 + lcc_11 + lcc_12 + lcc_30 + lcc_40 + lcc_50 + lcc_160 + lcc_170 + lcc_190 + lcc_210 + lcc_20 + lcc_60 + lcc_70 + lcc_100 + lcc_110 + lcc_120 + lcc_121 + lcc_130 + lcc_61 + lcc_62 + lcc_153 + lcc_180 + lcc_150 + lcc_200 + lcc_122 + lcc_80 + lcc_90 +  lcc_202 + lcc_201 + lcc_152 + lcc_81 + lcc_71 + lcc_72 +
## temp_mean + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations +
# prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations +
#     prop_change
##
## #### Linear regression:####
lm_rcp <- recipe(
    delta_std ~ temp_mean + temp_slope + temp_var + prec_mean + prec_slope +
        prec_var + prop_change + biome + burn_area,
    data = train_data) %>%
    step_filter(biome != "Rock and Ice") %>%
    step_log(burn_area ,signed = TRUE) %>% 
    #step_filter(n_ews == 0 | n_ews >1) %>%
    #step_rm(contains("var")) %>%
    step_dummy(biome) %>%
    step_BoxCox(prop_change) %>% #, starts_with("prec")
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())


lm_model <- linear_reg() %>%
    set_engine("lm")

lm_workflow <- workflow() %>%
    add_model(lm_model) %>%
    add_recipe(lm_rcp)

tic()
lm_fit <- fit(lm_workflow, train_data)
toc()


pull_workflow_fit(lm_fit) %>% broom::tidy()


folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
fit_res <- lm_workflow %>%
    fit_resamples(
        resamples = folds,
        control = keep_pred)
toc()

collect_metrics(fit_res)

fit_res$.predictions %>% bind_rows() %>%
    ggplot(aes(.pred, delta_std)) +
    geom_point(size = .2, alpha = .3) +
    geom_abline() +
    coord_obs_pred()

#### Logistic regression ####
####
#### Logistic model ####
#temp_mean + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations +
# prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations +
#     prop_change + biome + lcc_10 + lcc_11 + lcc_12 + lcc_20 + lcc_30 + lcc_40 + lcc_50 + lcc_60 +  lcc_61 + lcc_62 + lcc_70 + lcc_71 + lcc_72 + lcc_80 + lcc_81 + lcc_82 + lcc_90 + lcc_100 + lcc_110 + lcc_120 + lcc_121 + lcc_122 + lcc_130 + lcc_140 + lcc_150 + lcc_151 + lcc_152 + lcc_153 + lcc_160 + lcc_170 +  lcc_180 +  lcc_190 + lcc_200 + lcc_201 +  lcc_202 + lcc_210 _ lcc_220
log_rcp <- recipe(
    detected ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + 
        prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + 
        prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + 
        lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 +
      burn_area + biome ,
    data = train_data) %>%
    step_filter(biome != "Rock and Ice") %>%
    step_log(burn_area, signed = TRUE) %>% 
    #step_rm(contains("var")) %>%
    step_dummy(biome) %>%
    step_BoxCox(prop_change, starts_with("lcc")) %>%  #
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    #step_interact(terms = ~ prec_var:temp_var) %>%
    themis::step_downsample(detected)

## Interactions do not improve fit

log_model <- logistic_reg() %>%
    set_engine("glm", family = "binomial")

log_workflow <- workflow() %>%
    add_model(log_model) %>%
    add_recipe(log_rcp)

tic()
log_fit <- fit(log_workflow, train_data)
toc()

tic()
reg_df <- pull_workflow_fit(log_fit) %>% 
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) 
toc() # 51sec


g1 <- reg_df %>% 
    mutate(response = "Terrestrial Ecosystem Respiration") %>%
    mutate(p_value = ifelse(p.value > 0.05, "p > 0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%
    mutate(p_value = as_factor(p_value) %>% 
               fct_relevel(., levels = c("p<0.01", "p<0.05", "p > 0.05"))) %>% 
    mutate(type = case_when(
        str_detect(term, "temp_|prec_|burn_") ~ "Climate",
        str_detect(term, "lcc_|prop_change") ~ "Land cover class",
        str_detect(term, "biome") ~ "Biomes",
        str_detect(term, "Intercept") ~ ".")) %>% 
    mutate(term = str_remove_all(term, "biome_"), 
           term = str_replace(term, "lcc_80", "Needleleaved deciduous closed to open trees"),
           term = str_replace(term, "lcc_71", "Needleleaved evergreen closed to open trees (100-40%)"),
           term = str_replace(term, "lcc_61", "Broadleaved evergreen closed to open trees (100-40%)"),
           term = str_replace(term, "lcc_40", "Natural and semi-natural primarily terrestrial vegetation \n cultivated and managed terrestrial areas"),
           term = str_replace(term, "lcc_30", "Cultivated and managed terrestrial areas"),
           term = str_replace(term, "lcc_202", "Unconsolidated bare areas"),
           term = str_replace(term, "lcc_200", "Bare areas"),
           term = str_replace(term, "lcc_190", "Urban areas"),
           term = str_replace(term, "lcc_130", "Grassland"),
           term = str_replace(term, "lcc_11", "Rainfed herbaceous crops"),
           term = str_replace(term, "lcc_100", "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)"),
           term = str_replace(term, "lcc_10", "Rainfed shrub crops, tree crops, herbaceous crops"),
           term = str_replace(term, "burn_area", "Burned area")
           ) %>% 
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

ggsave(filename = "figures/fig_regression_TER.png", plot = g1,
       device = "png", dpi = 300,
       width = 4, height = 5, units = "in")


folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
log_res <- log_workflow %>%
    fit_resamples(
        resamples = folds,
        control = keep_pred)
toc() # 12s

collect_metrics(log_res) # accuracy 75.9%, roc_auc 0.68
# roc_curve(fit_res$.predictions[[1]], truth = detected, .pred_class)
# the model is not good, and is not improved by lcc.
# accuracy 0.67, roc_auc 0.73 with lcc
# accuracy 0.67, roc_auc 0.725 without lcc but with all the correlated climate variables
# accuracy 0.65, roc_auc 0.704 only with temp_mean + temp_slope + temp_var + prec_mean + prec_slope + prec_var + prop_change + biome
# accuracy 0.612, roc_auc 0.646 with burn area

log_fit$fit$fit

log_auc <- log_res %>%
    collect_predictions() %>%
    roc_curve(detected, .pred_TRUE) %>%
    autoplot()

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

rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
    set_mode("classification") %>%
    set_engine("ranger", importance = "impurity")

rf_workflow <- workflow() %>%
    add_model(rf_model) %>%
    add_recipe(rf_rcp)

# rf_res <-
#   rf_workflow %>%
#   tune_grid(resamples = folds,
#             grid = 25,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(roc_auc))


tic()
rf_fit <- fit(rf_workflow, train_data)
toc() # 57s


tic()
rf_res <- rf_workflow %>%
    fit_resamples(
        resamples = folds,
        control = keep_pred)
toc() # 621.442secs

collect_metrics(rf_res)


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

#### regularized regression ####
# same specs as logistic one
lasso_rcp <- recipe(
  detected ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 + lat + 
    biome,
        data = train_data) %>%
    step_filter(biome != "Rock and Ice") %>%
    #step_rm(contains("var")) %>%
    step_dummy(biome) %>%
    step_BoxCox(prop_change) %>%  #, starts_with("lcc")
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    step_interact(terms = ~ temp_std_annual_cycle:lat) %>%
    themis::step_downsample(detected) %>%
    prep(training = train_data, retain = TRUE)

lasso_model <- logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet")

parms <- parameters(lasso_model)

lasso_grid <- grid_regular(parms, levels = 3)
  
  #tibble(penalty = 10^seq(-4, -1, length.out = 30))

lasso_workflow <- workflow() %>%
    add_model(lasso_model) %>%
    add_recipe(lasso_rcp)



tic()
lasso_fit <- fit(lasso_workflow, train_data)
toc() #2s

pull_workflow_fit(lasso_fit) %>% 
  broom::tidy() %>% # conf.int = TRUE, exponentiate = TRUE
    mutate(p_value = ifelse(p.value > 0.05, "p > 0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%
    ggplot(aes(estimate, term)) +
    geom_point(aes(shape = as_factor(p_value), color = as_factor(p_value))) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = as_factor(p_value))) +
    geom_hline(xintercept = 1) +
    theme_light()


folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
lasso_res <- lasso_workflow %>%
    tune_grid(
        resamples = folds,
        control = control_grid(save_pred = TRUE),
        grid = lasso_grid,
        metrics = metric_set(roc_auc)
    )
toc()  #23s

lasso_res %>% collect_metrics() %>%
    ggplot(aes(x = penalty, y = mean)) +
    geom_point() +
    geom_line() +
    ylab("Area under the ROC Curve") +
    scale_x_log10(labels = scales::label_number())


lasso_res$.notes

collect_metrics(lasso_res)

### Maybe it can be tuned and improved: accuracy 0.6, roc_auc 0.64


#### Partial least squares ####
pls_rcp <- recipe(
  delta_std ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + 
    prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + 
    prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + 
    lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 +
    biome ,
  data = train_data) %>%
  step_filter(biome != "Rock and Ice") %>%
  #step_rm(contains("var")) %>%
  step_dummy(biome) %>%
  step_BoxCox(prop_change, starts_with("lcc")) %>%  #
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) #%>%
  #step_interact(terms = ~ prec_var:temp_var) %>%
  # themis::step_downsample(detected)

pls_model <- plsmod::pls(num_comp = 2, predictor_prop = 1) %>%
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
toc() # 12s

collect_metrics(pls_res)
## rmse 0.98, rsq 0.02: doesn't work at all.

pls_fit %>% tidy()


#### Marine regressions ####
#### Logistic model ####
log_rcp <- recipe(
    detected ~ temp_mean + temp_slope + temp_var , data = train_data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric())


log_model <- logistic_reg() %>%
    set_engine("glm", family = "binomial")

log_workflow <- workflow() %>%
    add_model(log_model) %>%
    add_recipe(log_rcp)

tic()
log_fit <- fit(log_workflow, train_data)
toc()

pull_workflow_fit(log_fit) %>% broom::tidy() %>%
    mutate(p_value = ifelse(p.value > 0.05, "p > 0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%
    ggplot(aes(estimate, term)) +
    geom_point(aes(shape = as_factor(p_value))) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error))


folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
log_res <- log_workflow %>%
    fit_resamples(
        resamples = folds,
        control = keep_pred)
toc() # 12s

collect_metrics(log_res) # accuracy 75.9%, roc_auc 0.68
# roc_curve(fit_res$.predictions[[1]], truth = detected, .pred_class)
# the model is not good, and is not improved by lcc.
log_res

#####
lm_rcp <- recipe(
    abs_delta_std ~ temp_mean + temp_slope + temp_var  , data = train_data) %>%
    step_interact(terms = ~ temp_slope:temp_var) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())


lm_model <- linear_reg() %>%
    set_engine("lm")

lm_workflow <- workflow() %>%
    add_model(lm_model) %>%
    add_recipe(lm_rcp)

tic()
lm_fit <- fit(lm_workflow, train_data)
toc()


pull_workflow_fit(lm_fit) %>% broom::tidy()


folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
fit_res <- lm_workflow %>%
    fit_resamples(
        resamples = folds,
        control = keep_pred)
toc()

collect_metrics(fit_res)

fit_res$.predictions %>% bind_rows() %>%
    ggplot(aes(.pred, abs_delta_std)) +
    geom_point(size = .2, alpha = .3) +
    geom_abline() +
    coord_obs_pred()
