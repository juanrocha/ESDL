## putting it together
library(tidyverse)
library(tictoc)
library(rworldmap)
library(rsample)
# library(cleangeo)
data("coastsCoarse")

## the aim of this script is to select pixels (time series) of places where early warnings were detected and a similar amount of places where EWS were not detected per ecosystem.

#### Terrestrial systems ####
# df is the data frame with min, max and diff per statistic
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200917_summary_gpp.RData')
# df_ews is the data frame with pixes where ews were detected
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200917_detected_gpp.RData')
#
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200918_gpp_segmented_results.RData')
# dataset with biomes:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')


## This is the number of pixels detected per ecosystem.
sample_num <- df_ews %>%
  ungroup() %>%
  group_by(biome) %>%
  filter(n_ews > 1) %>%
  tally()

## Two options: do it with as many timeseries as possible, so at least 1 ews detected. Or, select at least 2 or more ews selected to reduce noise (more centainty about resilience loss), it would reduce the number of time series and computation time (but also sample size)
df_ews %>% ungroup() %>% select(biome, n_ews) %>% table()

# a dataframe to sample non-detected pixels
df_sampling <- df %>%
  select(lon, lat) %>%
  unique() %>%
  left_join(df_biomes) %>%
  left_join(df_ews %>% select(lon,lat, n_ews)) %>%
  filter(is.na(n_ews))

df_sampling %>%
  ungroup() %>%
  group_by(biome) %>%
  tally()

sample <- df_sampling %>%
  ungroup() %>%
  group_by(biome) %>%
  slice_sample(prop = 0.2, replace = FALSE) %>%
  full_join(df_ews %>%
    filter(n_ews > 1) %>%
    select(lon,lat, biome_code, biome, n_ews)
  )


sample %>%
  mutate(detected =!is.na(n_ews)) %>%
  group_by(biome, detected) %>%
  tally() %>%
  pivot_wider(names_from = detected, values_from = n)

write_csv(sample, 
          path = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sample_pixels_gpp.csv")






#### Marine systems ####

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/20921_segmented_chlorA.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200918_detected_chlorA.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200918_summary_chlorA.RData')

### Visualizations:

px_results %>%
  filter(stat == "ac1") %>%
  ggplot(aes(lon, lat)) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  geom_tile(aes(fill = slope2)) +
  scale_fill_viridis_c(option = "C")

## Nice map with time clustering
px_results %>% 
  mutate(date = lubridate::as_date(time)) %>% 
  #filter(stat == "kur") %>%
  ggplot(aes(lon, lat)) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  geom_tile(aes(fill = date)) +
  scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  
                       na.value = "grey50", trans = "date") +
  facet_wrap(~stat) +
  theme_light() +
  theme(
    legend.position = c(0.85,0.25), 
    legend.direction = "vertical",
    legend.box = "vertical", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  )

# quartz.save(file = "figures/figS3_temporal_coherence_GPP.png", type = "png", 
#             width = 7, height = 5, dpi = 800)


px_results %>%
  #filter(stat == "kur") %>%
  ggplot(aes(lon, lat)) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  geom_tile(aes(fill = break_std_error))+
  scale_fill_viridis_c(option = "C") +
  facet_wrap(~stat) +
  theme_light()

## makes sense, the smaller the difference, the higher the error on the estimate.
px_results %>%
  ggplot(aes(break_std_error, diff)) +
  geom_point(size = 0.1, alpha = 0.25) +
  facet_wrap(~stat, scales = "free")

px_results %>%
  ggplot(aes(break_pt)) +
  geom_density() +
  # geom_point(size = 0.1, alpha = 0.25) +
  facet_wrap(~stat, scales = "free")


px_results
