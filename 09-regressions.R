## This script test a series of regressions to understand what explains the detection of early warnings.
## J201127: currently it works with a subsample of the data on one variable. 
## Next step, combine data for explanatory variables (landuse and fires)

library(tidyverse)
library(tictoc)
library(corrgram)
library(tidymodels)

set.seed(777)

## First load the summary results of the ews:
# df is the data frame with min, max and diff per statistic. ~150MB in memory
# load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_summary_gpp_log.RData') 

# new analysis with deltas: deltas is a df with the delta and abruptness per statistic. 19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_gpp_log.RData")
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_GPP_log.RData")

## then the sample file used to extract the samples: this is pixels where ews were detected and pixels witout detection. The sampling is stratified with the same proportion per biome.
sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_delta_gpp.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        #biome_code = col_double(),
        biome = col_character(),
        n_ews = col_double()
    ))

# J201017: Remember that the order of the list is the same as the order of the
# sample dataset by lon lat.
# prop <- sample %>%
#     select(biome) %>%
#     group_by(biome) %>%
#     tally() %>%
#     mutate(prop = n / dim(sample)[1]) %>%
#     pull(prop)

## select a very small sample of the data for exploration and visualizations:
dat <- sample %>%
    mutate(id = row_number()) %>%
    group_by(biome) %>%
    slice_sample(prop = 0.1)

## Now load explanatory variables and reduce them immediately to the pixels on the sample
# load precipitation data: 2.4Gb
tic()
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_precipitation_FFT_GPP.RData')
toc() #13s
## reduce the out object to only the pixels sampled
out_prec <- out[dat$id] 
rm(out)

# load temperature data: 2.4Gb
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_temperature_FFT_GPP.RData')

## reduce the out object to only the pixels sampled
out_temp <- out[dat$id] 
rm(out)


## reduce df to only the pixels you sampled:
tic()
deltas <- deltas %>%
  right_join(dat)
toc()

tic()
out_temp <- out_temp %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  rename(
    temp_trend = trend,
    temp_longtv = long_term_variability,
    temp_annualc = annual_cycle,
    temp_fasto = fast_oscillations
  )
toc() # 5.4s

tic()
out_prec <- out_prec %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  rename(
    prec_trend = trend,
    prec_longtv = long_term_variability,
    prec_annualc = annual_cycle,
    prec_fasto = fast_oscillations
  )
toc() # 5.7s


tic()
out <- full_join(out_prec, out_temp)
toc() # 49s


tic()
df_out <- out %>%
  rowwise() %>%
  mutate(
    rain = sum(prec_annualc, prec_fasto, prec_longtv, prec_trend, na.rm = TRUE),
    temp = sum(temp_annualc, temp_fasto, temp_longtv, temp_trend, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(lon, lat) %>%
  summarise(
    mean_temp = mean(temp),
    temp_std_longterm = sd(temp_longtv),
    temp_std_annual_cycle = sd(temp_annualc),
    temp_std_fast_oscillations = sd(temp_fasto),
    mean_rain = mean(rain),
    prec_std_longterm = sd(prec_longtv),
    prec_std_annual_cycle = sd(prec_annualc),
    prec_std_fast_oscillations = sd(prec_fasto),
    .groups = "keep"
  )
toc() #31s | 1039.744s, 17min

tic()
df_out$temp_slope <- out_temp %>% 
  group_by(lon, lat) %>%
  group_split() %>%
  map_dbl( ~ lm(temp_trend ~ date, data = .x) %>%
             broom::tidy() %>%
             # this is the slope:
             filter(term == "date") %>% pull(estimate)
  )
toc()  # 25s | 24min

tic()
df_out$prec_slope <- out_prec %>% 
  group_by(lon, lat) %>%
  group_split() %>%
  map_dbl( ~ lm(prec_trend ~ date, data = .x) %>%
             broom::tidy() %>%
             # this is the slope:
             filter(term == "date") %>% pull(estimate)
  )
toc()  # 25s | 16min

## check result: ggplot(df_out, aes(temp_slope)) + geom_density()
### J210118: Do I need to do feature engineering e.g. rescaling again units of T
###  and precipitation due to the different units and magnitudes?

## now join the two:
tic()
deltas <- deltas %>%
  # pivot_wider(
  #   names_from = c("stat", "feature"), values_from = value,
  #   names_sep = "_") %>%
    ungroup() %>% #skimr::skim()
    left_join(df_out) 
toc() #1s

### Now you can fee some memory:
rm(out_prec)
rm(out_temp)
###


# load land use
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_land_cover_GPP.RData')

df_land <- dat %>% 
  left_join(prop_change_df)


df_land2 <- dat %>% 
  left_join(
    pxl_land_cover_change %>%
      select(-c(2,3)) %>%
      ungroup() %>% 
      group_by(lon, lat) %>% 
      pivot_wider(names_from = lccs_class, values_from = pxl_change, values_fill = 0,
                  names_prefix = "lcc_") 
  )

df_land <- left_join(df_land, df_land2)

deltas <- left_join(deltas, df_land)

save(deltas, file = "Results/210329_processed_reg_data_GPP.RData")
## clean up a bit
rm(df_land, df_land2, out, prop_change_df, pxl_land_cover_change, sample)


#### Data exploration to decide on variables ####
deltas <- deltas %>% mutate(detected = n_ews != 0)


deltas %>%
  filter(n_ews == 0 | n_ews >1) %>% 
  ggplot(aes(temp_std_fast_oscillations, abs(delta_std))) +
  geom_point(aes(color = biome), show.legend = FALSE, size = 0.5, alpha = .4) +
  geom_smooth() +
  facet_wrap(biome~detected)

## correlation matrix for deltas of EWS
deltas %>% ungroup() %>%
  select(starts_with("delta"), detected) %>% 
  #corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
  GGally::ggpairs(
    aes(fill = detected, color = detected),
    lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## Note that you can use the absolute value to push all the distribution to positive numbers.
## It seems symmetric without abs.
deltas %>% 
  filter(n_ews == 0 | n_ews > 2) %>% 
  select(starts_with("delta"), detected) %>% 
  pivot_longer(cols = starts_with("delta"), names_to = "stat", values_to = "value") %>% 
  ggplot(aes(abs(value))) +
  geom_density(aes(fill = detected), alpha = 0.5) +
  facet_wrap(~stat, scales = "free")

## For rain, one could drop the slope (very little variability) and the mean_rain (high corr)
deltas %>% select(prop_change, contains("prec"), mean_rain, detected, delta_std) %>% 
  # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
  GGally::ggpairs(
    aes(fill = detected, color = detected),
    lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## For temperature, there is a lot of correlation, but the distributions are quite different
## between detected and non-detected. That can be accounted by biome (or lat, lon controls).
## The hypothesis is whether is slow change or stochastic processes, for T I think one should leave them all in.
deltas %>% select(prop_change, contains("temp"), mean_temp, detected) %>% 
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

## Rock and ice should be dropped, maybe inland water?
deltas %>% 
  ggplot(aes(y = sqrt(delta_std), x = (temp_slope))) +
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

deltas <- deltas %>% 
  mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) 

deltas <- deltas %>% 
  mutate(detected = as_factor(detected)) 

deltas <- deltas %>%
  left_join(df_delta_detected)
# 
# df <- df %>%
#   filter(biome != "Rock and Ice") %>%
#   select( -lcc_82, -lcc_220)


#### Regressions & model fitting #### 

# Based on the correlograms above, I will exclude terms that are strongly correlated.
# For precipitation prec_std_longterm.
# For temperature: I don't know, strong corr in mean_temp, temp_std_annual_cycle, and fast_oscillations
# In a way temp variability will be explained as well by the fixed effects of biomes.
# See for example: the lower the temp the higher the detection, but the higher the std_fast_oscillations,
# the higher the detection.


## Data split
data_split <- initial_split(
  deltas %>% 
    filter(n_ews == 0 | n_ews >1, biome == "Boreal Forests/Taiga"), 
  strata = biome, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)

## data-processing and feature engineering:
## land-use terms: lcc_10 + lcc_11 + lcc_12 + lcc_30 + lcc_40 + lcc_50 + lcc_160 + lcc_170 + lcc_190 + lcc_210 + lcc_20 + lcc_60 + lcc_70 + lcc_100 + lcc_110 + lcc_120 + lcc_121 + lcc_130 + lcc_61 + lcc_62 + lcc_153 + lcc_180 + lcc_150 + lcc_200 + lcc_122 + lcc_80 + lcc_90 +  lcc_202 + lcc_201 + lcc_152 + lcc_81 + lcc_71 + lcc_72 +

#### Linear regression:####
ews_rcp <- recipe(
  abs_delta_std ~ mean_temp + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + 
    prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + 
    prop_change + biome, data = train_data) %>%
  step_filter(biome != "Rock and Ice") %>% 
  #step_filter(n_ews == 0 | n_ews >1) %>% 
  step_dummy(biome) %>% 
  step_BoxCox(prop_change) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())


lm_model <- linear_reg() %>%
  set_engine("lm")

lm_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ews_rcp)

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

# A tibble: 2 x 6
# .metric .estimator   mean     n  std_err .config             
# <chr>   <chr>       <dbl> <int>    <dbl> <chr>               
# 1 rmse    standard   0.0679    10 0.000567 Preprocessor1_Model1
# 2 rsq     standard   0.247     10 0.00949  Preprocessor1_Model1

fit_res$.predictions %>% bind_rows() %>% 
  ggplot(aes(.pred, abs_delta_std)) +
  geom_point(size = .2, alpha = .3) +
  geom_abline() +
  coord_obs_pred()

## Linear model has very low R^2 =0.2 and the RMSE is 0.88 meaning that the average error is 0.88 standard deviations (that's the units of the EWS), which is too high. Can a logistic approach do better?

#### Logistic model ####
ews_rcp <- recipe(
  detected ~ mean_temp + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + 
    prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + 
    prop_change + biome + lcc_10 + lcc_11 + lcc_12 + lcc_30 + lcc_40 + lcc_50 + lcc_160 + lcc_170 + lcc_190 + lcc_210 + lcc_20 + lcc_60 + lcc_70 + lcc_100 + lcc_110 + lcc_120 + lcc_121 + lcc_130 + lcc_61 + lcc_62 + lcc_153 + lcc_180 + lcc_150 + lcc_200 + lcc_122 + lcc_80 + lcc_90 +  lcc_202 + lcc_201 + lcc_152 + lcc_81 + lcc_71 + lcc_72 , data = train_data) %>%
  step_filter(biome != "Rock and Ice") %>% 
  step_dummy(biome) %>% 
  step_BoxCox(prop_change, starts_with("lcc")) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) #%>%  step_downsample(detected)


log_model <- logistic_reg() %>%
  set_engine("glm")

log_workflow <- workflow() %>% 
  add_model(log_model) %>% 
  add_recipe(ews_rcp)

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
fit_res <- log_workflow %>% 
  fit_resamples(
    resamples = folds, 
    control = keep_pred)
toc()

collect_metrics(fit_res) # accuracy 75.9%, roc_auc 0.68
# roc_curve(fit_res$.predictions[[1]], truth = detected, .pred_class)

fit_res

#### Boosted trees? ####




####
y_vars <- str_subset(names(df), pattern = "diff")
x_vars <- str_subset(names(df), pattern = "temp|prec|rain|prop_change|lcc_") %>%
  str_flatten (., " + ")
out_lm <- list()

tic()
out_lm <- map(.x = y_vars, ~lm( 
    formula = 
      eval(sym(.x)) ~ mean_temp + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + mean_rain + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope  + prop_change + lcc_10 + lcc_11 + lcc_12 + lcc_30 + lcc_40 + lcc_50 + lcc_160 + lcc_170 + lcc_190 + lcc_210 + lcc_20 + lcc_60 + lcc_70 + lcc_100 + lcc_110 + lcc_120 + lcc_121 + lcc_130 + lcc_61 + lcc_62 + lcc_153 + lcc_180 + lcc_150 + lcc_200 + lcc_122 + lcc_80 + lcc_90 +  lcc_202 + lcc_201 + lcc_152 + lcc_81 + lcc_71 + lcc_72 + as.factor(biome),
    data = df
))
toc()


summary(out_lm[[5]])
# plot(fit)


## Detected is a Y/N dummy, so I need a logistic regression:
tic()
fit <- glm(
  formula = 
    detected ~ 
    mean_temp + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + mean_rain + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope  + prop_change + lcc_10 + lcc_11 + lcc_12 + lcc_30 + lcc_40 + lcc_50 + lcc_160 + lcc_170 + lcc_190 + lcc_210 + lcc_20 + lcc_60 + lcc_70 + lcc_100 + lcc_110 + lcc_120 + lcc_121 + lcc_130 + lcc_61 + lcc_62 + lcc_153 + lcc_180 + lcc_150 + lcc_200 + lcc_122 + lcc_80 + lcc_90 +  lcc_202 + lcc_201 + lcc_152 + lcc_81 + lcc_71 + lcc_72 + 
    as.factor(biome), family = binomial(link = "logit"), 
  data = df
)
toc()
summary(fit)

tic()
df_lm <- out_lm %>%
  map(tidy) %>%
  map2(., y_vars,  function(x,y) {
    x$response <- y
    return(x)}) %>%
  bind_rows() %>%
  mutate(term = str_remove_all(term, "as.factor\\(biome\\)"))
toc()

df_glm <- fit %>%
  tidy() %>%
  add_column(response = "detect") %>% 
  mutate(term = str_remove_all(term, "as.factor\\(biome\\)"))

df_lm %>% bind_rows(df_glm) %>%
  mutate(term = factor(term, levels = rev(unique(term)))) %>%
  mutate(conf.high = estimate + std.error,
         conf.low = estimate - std.error,) %>%
  mutate(p_value = ifelse(
    p.value < 0.01, "< 0.01" ,
    ifelse(p.value < 0.05, "< 0.05", "> 0.05")
  )) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "grey84", linetype = 2, size = 0.5) +
  geom_point(aes(shape = p_value, color = p_value), size = 2, show.legend = TRUE) +
  scale_shape_manual(name = "p value", values = c(19,7,1)) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, height = .25, color = p_value), 
    size = 0.3) +
  scale_x_continuous(minor_breaks = NULL, breaks = scales::pretty_breaks(n=3)) +
  scale_color_manual(name = "p value",values = c("dodgerblue", "orange", "purple")) +
  theme_light(base_size = 13) +
  theme(legend.position = "bottom", 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 12) ) +
  facet_grid(~ response, scales = "free", switch = "y", space = "free_y")
  

## J210119: Land use is not working. I suspect I have issues with unbalanced dataset
## I sampled with biome as strata but if one run the regression on ac1 or std
## the stratification might change. Need to go back to the theory and the whiteboard.

map(out_lm, mosaic::rsquared)


fit %>% 
  tidy() %>%
  mutate(term = factor(term, levels = rev(unique(term)))) %>%
  mutate(conf.high = estimate + std.error,
         conf.low = estimate - std.error,) %>%
  mutate(p_value = ifelse(
    p.value < 0.01, "< 0.01" ,
    ifelse(p.value < 0.1, "< 0.1", "> 0.1")
  )) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "grey84", linetype = 2, size = 0.5) +
  geom_point(aes(shape = p_value, color = p_value), size = 2, show.legend = TRUE) +
  scale_shape_manual(name = "p value", values = c(19,7,1)) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, height = .25, color = p_value), 
    size = 0.3) +
  scale_x_continuous(minor_breaks = NULL, breaks = scales::pretty_breaks(n=3)) +
  scale_color_manual(name = "p value",values = c("dodgerblue", "orange", "purple")) +
  theme_light(base_size = 13) +
  theme(legend.position = "bottom", 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 12) )
  


#### some visualizations ####

df %>% 
  ggplot(aes(value)) +
  geom_density(
  aes(fill = biome, color = biome), alpha = 0.3,
  show.legend = FALSE) +
  facet_wrap(stat ~ feature, scales = "free") +
  theme_light()

## plotting the Fourier tranform of one pixel.
out_prec[[1]] %>%
  pivot_longer(cols  = 1:4, names_to = "layer", values_to = "value") %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~ layer, ncol = 1, scales = "free_y") +
  theme_light()