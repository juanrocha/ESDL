## This script test a series of regressions to undestand what explains the detection of early warnings.
## J201127: currently it works with a subsample of the data on one variable. 
## Next step, combine data for explanatory variables (landuse and fires)

library(tidyverse)
library(tictoc)
library(corrgram)
library(tidymodels)

set.seed(777)

## First load the summary results of the ews:
# df is the data frame with min, max and diff per statistic. ~150MB in memory
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_summary_gpp_log.RData') 

## then the sample file used to extract the samples: this is pixels where ews were detected and pixels witout detection. The sampling is stratified with the same proportion per biome.
sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_gpp.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        biome_code = col_double(),
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
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_terrestrial_precipitation_FFT_4GPP.RData')
toc() #13s
## reduce the out object to only the pixels sampled
out_prec <- out[dat$id] 
rm(out)

# load temperature data: 2.4Gb
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_terrestrial_temperature_FFT_4GPP.RData')

## reduce the out object to only the pixels sampled
out_temp <- out[dat$id] 
rm(out)


## reduce df to only the pixels you sampled:
df <- df %>%
  right_join(dat)

out_temp <- out_temp %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  rename(
    temp_trend = trend,
    temp_longtv = long_term_variability,
    temp_annualc = annual_cycle,
    temp_fasto = fast_oscillations
  )

out_prec <- out_prec %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  rename(
    prec_trend = trend,
    prec_longtv = long_term_variability,
    prec_annualc = annual_cycle,
    prec_fasto = fast_oscillations
  )

tic()
out <- full_join(out_prec, out_temp)
toc()


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
toc() #31s

tic()
df_out$temp_slope <- out_temp %>% 
  group_by(lon, lat) %>%
  group_split() %>%
  map_dbl( ~ lm(temp_trend ~ date, data = .x) %>%
             broom::tidy() %>%
             # this is the slope:
             filter(term == "date") %>% pull(estimate)
  )
toc()  # 25s

tic()
df_out$prec_slope <- out_prec %>% 
  group_by(lon, lat) %>%
  group_split() %>%
  map_dbl( ~ lm(prec_trend ~ date, data = .x) %>%
             broom::tidy() %>%
             # this is the slope:
             filter(term == "date") %>% pull(estimate)
  )
toc()  # 25s

## check result: ggplot(df_out, aes(temp_slope)) + geom_density()
### J210118: Do I need to do feature engineering e.g. rescaling again units of T
###  and precipitation due to the different units and magnitudes?

## now join the two:
tic()
df <- df %>%
  pivot_wider(
    names_from = c("stat", "feature"), values_from = value,
    names_sep = "_") %>%
    ungroup() %>% #skimr::skim()
    left_join(df_out) 
toc()


df %>%
  mutate(detect = !is.na(n_ews)) %>%
  ggplot(aes(std_diff, temp_std_fast_oscillations)) +
  geom_point(aes(color = biome), show.legend = FALSE) +
  facet_wrap(.~detect, ncol = 1)

## correlation matrix with smooths
df %>% ungroup() %>%
  mutate(detect = !is.na(n_ews)) %>%
  select(std_diff, ac1_diff, kur_diff, skw_diff, fd_diff, detect) %>% 
  #corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
  GGally::ggpairs(
    aes(fill = detect, color = detect),
    lower = list(continuous = GGally::wrap("points", alpha = 0.15)))


df %>% ungroup() %>%
  mutate(detect = !is.na(n_ews)) %>%
  select(contains("prec"), mean_rain, detect) %>% 
  # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
  GGally::ggpairs(
    aes(fill = detect, color = detect),
    lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

df %>% ungroup() %>%
  mutate(detect = !is.na(n_ews)) %>%
  select(contains("temp"), mean_temp, detect) %>% 
  # corrgram(upper.panel = panel.pts, lower.panel = panel.pie)
  GGally::ggpairs(
    aes(fill = detect, color = detect),
    lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

df %>% 
  mutate(detected = !is.na(n_ews)) %>%
  ggplot(aes(y = std_diff, x = (temp_std_fast_oscillations))) +
  geom_point(aes(color = n_ews), size = 0.5, alpha = 0.6) +
  geom_smooth(aes(group = detected)) +
  facet_wrap(~biome, scales = "free") +
  theme_light()



# load land use
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_land_cover_4GPP.RData')

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

df <- left_join(df, df_land)

## regression 
## I can do this later | no yet, dat is just one of the response variables.
# data_split <- initial_split(sample, strata = biome, prop = 3/4)
# train_data <- training(data_split)
# test_data <- testing(data_split)
# 
# 
# Based on the correlograms above, I will exclude terms that are strongly correlated.
# For precipitation prec_std_longterm.
# For temperature: I don't know, strong corr in mean_temp, temp_std_annual_cycle, and fast_oscillations
# In a way temp variability will be explained as well by the fixed effects of biomes.
# See for example: the lower the temp the higher the detection, but the higher the std_fast_oscillations,
# the higher the detection.
df <- df %>% mutate(detected = !is.na(n_ews))

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