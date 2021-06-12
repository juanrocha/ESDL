library(tidyverse)
library(tidymodels)

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

## Data split
set.seed(777)
data_split <- initial_split(
    deltas %>% filter(n_ews == 0 | n_ews >1), #, biome == "Boreal Forests/Taiga"
    strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)


#### Multiple regression ####

ews_rcp <- recipe(
    delta_std ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 +
        burn_area + biome + lat + lon,
    data = train_data) %>%
    step_filter(biome != "Rock and Ice") %>%
    step_log(burn_area, signed = TRUE) %>% 
    #step_rm(contains("var")) %>%
    step_dummy(biome) %>%
    step_BoxCox(prop_change) %>%  #, starts_with("lcc")
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric()) %>%
    #step_interact(terms = ~ prec_var:temp_var) %>%
    themis::step_downsample(detected)

