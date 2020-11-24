library(tidyverse)
library(tictoc)

# load temperature data
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_pixels_terrestrial_temperature_FFT.RData')

## load summary results for GPP
# df is the data frame with min, max and diff per statistic
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200917_summary_gpp.RData') # use the old non-log results for test

sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sample_pixels_gpp.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        biome_code = col_double(),
        biome = col_character(),
        n_ews = col_double()
    ))

# J201017: Remember that the order of the list is the same as the order of the
# sample dataset by lon lat.

prop <- sample %>%
  select(biome) %>%
  group_by(biome) %>%
  tally() %>%
  mutate(prop = n / dim(sample)[1]) %>%
  pull(prop)

## select a very small sample of the data for exploration and visualizations:

dat <- sample %>%
  mutate(id = row_number()) %>%
  group_by(biome) %>%
  slice_sample(prop = 0.05)

## reduce df to only the pixels you sampled:
df <- df %>%
  right_join(dat)

df %>% ggplot(aes(value)) +
  geom_density(
    aes(fill = biome, color = biome), alpha = 0.3,
    show.legend = FALSE) +
  facet_wrap(stat ~ feature, scales = "free") +
  theme_light()

out <- out[dat$id]

tic()
out <- out %>%
  map(janitor::clean_names)
toc()


out[[1]] %>%
  pivot_longer(cols  = 1:4, names_to = "layer", values_to = "value") %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~ layer, ncol = 1, scales = "free_y") +
  theme_light()

tic()
df_out <- out %>%
  bind_rows() %>%
  group_by(lon, lat) %>%
  rowwise() %>%
  mutate(temp = sum(trend, long_term_variability, annual_cycle, fast_oscillations, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(lon, lat) %>%
  summarise(
    mean_temp = mean(temp),
    std_longterm = sd(long_term_variability),
    std_annual_cycle = sd(annual_cycle),
    std_fast_oscillations = sd(fast_oscillations),
    .groups = "keep"
  )
toc()


## how to extract the slope for summarizing?
tic()
df_out$slope_temp <- out %>%
  map_dbl( ~ lm(trend ~ date, data = .x) %>%
    broom::tidy() %>%
    # this is the slope:
    filter(term == "date") %>% pull(estimate)
  )
toc()


df_out %>% ggplot(aes(slope_temp)) + geom_density()

## now join the two:
df <- df %>%
  pivot_wider(
    names_from = c("stat", "feature"), values_from = value,
    names_sep = "_") %>%
  left_join(df_out)

df %>%
  mutate(detect = !is.na(n_ews)) %>%
  ggplot(aes(std_diff, std_fast_oscillations)) +
  geom_point(aes(color = biome, alpha = detect))

## correlation matrix with smooths
