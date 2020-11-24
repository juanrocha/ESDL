## putting it together
library(tidyverse)
library(tictoc)
library(rworldmap)
library(rsample)
# library(cleangeo)
data("coastsCoarse")

## the aim of this script is to select pixels (time series) of places where early warnings were detected and a similar amount of places where EWS were not detected per ecosystem.

#### Terrestrial systems ####
## Because I'm using multiple datasets for terrestrial systems, first I need to combine them. So the sampling afterwards works for all of them.
# df is the data frame with min, max and diff per statistic
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_summary_gpp_log.RData')
df_gpp <- df %>%
  rename(value_gpp = value)

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201027_summary_lai_log.RData')
df_lai <- df %>%
  rename(value_lai = value)

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201029_summary_terrestrial_ecosystem_respiration_log.RData')
df_ter <- df %>%
  rename(value_ter = value)

object.size(df) %>% format("Mb")
rm(df)

df_all <- full_join(df_gpp, df_ter) %>%
  left_join(df_lai)
  # the left_join with LAI removes the Antarctica part of the dataset but keep them all comparable.

# df_ews is the data frame with pixes where ews were detected
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_detected_gpp_log.RData")
df_ews_gpp <- df_ews %>% add_column(metric = "gpp")

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201024_detected_terrestrial_ecosystem_respiration_log.RData')
df_ews_ter <- df_ews %>% add_column(metric = "ter")

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201024_detected_lai_log.RData')
df_ews_lai <- df_ews %>% add_column(metric = "lai")

rm(df_ews)

df_ews_all <- full_join(df_ews_gpp, df_ews_ter) %>%
  full_join(df_ews_lai)

df_ews_all %>% #ungroup() %>%
  select(-skw, -ac1, -std, -kur, -fd) %>%
  pivot_wider(names_from = metric, values_from = n_ews, values_fill = 0) %>%
  rowwise() %>%
  mutate(avrg = mean(c(gpp, ter, lai))) %>%
  arrange((avrg)) %>%
  ## filter pixels with at least 2 metrics identifying ews, and exclude Antartica to make the 3 metrcis comparable, Antartica data is only available for LAI and I have my doubts about quality of the product.
  filter(avrg > 0.4, lat > -65) %>%
  # ungroup() %>% group_by(biome) %>% tally()
  ## map visualizaton
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = avrg)) +
    geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
    scale_fill_viridis_c(option = "C") +
    theme_void()
  ## correlation visualisation:
    #GGally::ggpairs(columns = 5:7, aes(color = biome))

# df_ews_all %>%
#   ungroup() %>%
#   group_by(biome, metric) %>%
#   summarize(
#     ews_sum = sum(n_ews),
#     skw_sum = sum(skw, na.rm = TRUE) / ews_sum,
#     ac1_sum = sum(ac1, na.rm = TRUE) / ews_sum,
#     std_sum = sum(std, na.rm = TRUE) / ews_sum,
#     kur_sum = sum(kur, na.rm = TRUE) / ews_sum,
#     fd_sum = sum(fd, na.rm = TRUE) / ews_sum,
#     .groups = "keep"
#   ) %>%
#   pivot_longer(cols = 3:last_col(), names_to = "stat", values_to = "value") %>%
#   filter(stat != "ews_sum") %>%
#   ggplot(aes(stat, value)) +
#   geom_col(aes(fill = stat), show.legend = TRUE) +
#   facet_grid(biome ~ metric) +
#   coord_polar() +
#   theme_light(base_size = 6)


## J201104: One of the problems with combining the three variables for terresrial systemas is that the number of pixels increased substantially. df_all contain 210K pixels total. Pixels detected for GPP with more than one measure are ~25K, for TER about the same amount ~25K but not necessarily the same; while for LAI ~33K. But for all combined is >70K pixels (n_ews > 1)!!! It means that I need to sample other 70K of non-detected pixels and I'm not sure how much noise I'll inducing by doing so in the regressions.

## J201112: Keep the regression analysis separated. So produce one sampling file for each variable. It is attractive to process T and precipitation only once for all regressions. The problem is I'd need to download all the Fourier tranform data per variable, that's over 20Gb each.


# At the moment I'm not using the stats on segmented regression for the sampling. But interesting to combine them and see how much agreement is there over time.
# load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_gpp_log.RData')
#

# dataset with biomes:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')


## This is the number of pixels detected per ecosystem.
## To extract the same for different variables, you need to make sure to chage the df_ews_ with the right variable as well as the df_ below.
sample_num <- df_ews_ter %>%
  ungroup() %>%
  group_by(biome) %>% select(-metric) %>%
  filter(n_ews > 1) %>%
  tally() %>%
  left_join(
    df_ter %>%
      select(lon, lat) %>%
      unique() %>%
      left_join(df_biomes) %>%
      #left_join(df_marine) %>%
      left_join(df_ews_ter %>% select(lon,lat, n_ews)) %>%
      ungroup() %>% group_by(biome) %>%
      tally() %>%
      rename(totalpx = n)
  ) %>%
  mutate(prop = n / totalpx)

## Two options: do it with as many timeseries as possible, so at least 1 ews detected. Or, select at least 2 or more ews selected to reduce noise (more centainty about resilience loss), it would reduce the number of time series and computation time (but also sample size)
df_ews_clo %>% ungroup() %>% select(biome, n_ews) %>% table()

# a dataframe to sample non-detected pixels
df_sampling <- df_ter %>%
  select(lon, lat) %>%
  unique() %>%
  left_join(df_biomes) %>%
  # left_join(df_marine) %>%
  left_join(df_ews_ter %>% select(lon,lat, n_ews)) %>%
  filter(is.na(n_ews))

df_sampling %>%
  ungroup() %>%
  group_by(biome) %>%
  tally()

sample <- df_sampling %>%
  ungroup() %>%
  group_by(biome) %>%
  slice_sample(prop = 0.2, replace = FALSE) %>%
  full_join(df_ews_ter %>%
    filter(n_ews > 1) %>%
    select(lon,lat, biome_code, biome, n_ews)
  )


sample %>%
  mutate(detected =!is.na(n_ews)) %>%
  group_by(biome, detected) %>%
  tally() %>%
  pivot_wider(names_from = detected, values_from = n)

write_csv(sample,
          file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sample_pixels_ter.csv")

## repeat and extract samples for TER, LAI. Do all for ChlorA.


### Little visualization:
df_all %>%
  filter(feature == "diff") %>%
  ggplot(aes(value_ter, value_lai)) +
  geom_point(size = 0.2, alpha = 0.2) +
  #geom_smooth() +
  facet_wrap(~stat, scales = "free")



#### Marine systems ####

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

# load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_chlorA_log.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201024_detected_chlorA_log.RData')
df_ews_clo <- df_ews  %>% add_column(metric = "clo")

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201024_summary_chlorA_log.RData')
df_clo <- df

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


df_ews <- df_ews %>%
  select(-biome, -biome_code) %>%
  left_join(df_marine)

df_ews


### Note to self:
# the log dataset detects more pixels but htere is less pixels detected by all 5 metrics:
# > df_ews$n_ews %>% table()
# .
#     1     2     3     4     5
# 38638 15701  6932  2729   484
# > df_ews_log$n_ews %>% table()
# .
#     1     2     3     4     5
# 43964 16976  6293  1505   178
# df_ews_log is the same df_ews for the logged file, but to be able to compare I created a copied object with the _log and then loaded the non-log data.
