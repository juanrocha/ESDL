## This script test a series of regressions to undestand what explains the detection of early warnings.
## J201127: currently it works with a subsample of the data on one variable. Next step, combine data for explanatory variables (precipitation, temperature)

library(tidyverse)
library(tictoc)
library(corrgram)

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


out <- full_join(out_prec, out_temp)

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

## check result: ggplot(df_out, aes(prec_slope)) + geom_density()


## now join the two:
df <- df %>%
  pivot_wider(
    names_from = c("stat", "feature"), values_from = value,
    names_sep = "_") %>%
    ungroup() %>% #skimr::skim()
    left_join(df_out) 

df %>%
  mutate(detect = !is.na(n_ews)) %>%
  ggplot(aes(std_diff, temp_std_fast_oscillations)) +
  geom_point(aes(color = biome, alpha = detect))

## correlation matrix with smooths
df %>% ungroup() %>%
    select(std_diff, ac1_diff, kur_diff, skw_diff, fd_diff) %>% 
    corrgram(upper.panel = panel.pts, lower.panel = panel.pie)

df %>% ungroup() %>%
    select(contains("prec"), mean_rain) %>% 
    corrgram(upper.panel = panel.pts, lower.panel = panel.pie)

df %>% ungroup() %>%
  select(contains("temp"), mean_rain) %>% 
  corrgram(upper.panel = panel.pts, lower.panel = panel.pie)

df %>% 
    mutate(detected = !is.na(n_ews)) %>%
    ggplot(aes(y = std_diff, x = (temp_std_fast_oscillations))) +
    geom_point(aes(color = n_ews), size = 0.5, alpha = 0.6) +
  geom_smooth(aes(group = detected)) +
    facet_wrap(~biome, scales = "free") +
    theme_light()

## regression 



fit <- lm(
    formula = 
      fd_diff ~ 
      mean_temp + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + temp_slope 
    + mean_rain + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + prec_slope 
    + as.factor(biome)
    ,
    data = df %>% mutate(detected = !is.na(n_ews))
)

summary(fit)
plot(fit)


## Detected is a Y/N dummy, so I need a logistic regression:

fit <- glm(
  formula = 
    detected ~ 
    mean_temp + temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + temp_slope 
  + mean_rain + prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + prec_slope 
  + as.factor(biome)
  , family = binomial(link = "logit"), 
  data = df %>% mutate(detected = !is.na(n_ews))
)

summary(fit)













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