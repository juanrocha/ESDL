library(tidyverse)
library(tictoc)

# load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200921_summary_terrestrial_ecosystem_respiration.RData")

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201024_summary_chlorA_log.RData")


# dataset with biomes:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

# or run the 07-ecoregions script and merge with results data frame:
df <- left_join(df, df_biomes) # terrestrial
df <- left_join(df, df_marine) # marine

q90 <- df %>% group_by(biome, stat) %>%
  filter(feature == "diff") %>%
  summarise(q90 = quantile(value, prob = 0.9))

df <- left_join(df, q90) %>%
  mutate(q90_plus = value > q90)


## select the pixels where stark differences were detected:
# df_ews <- df %>%
#   filter(q90_plus == TRUE, feature == "diff") %>%
#   group_by(lon, lat, biome) %>% # ggplot(aes(biome)) + geom_bar() + coord_flip()
#   summarise(n_ews = n())

## this way is longer but I recover info about what EWS were detected.
df_ews <- df %>%
  filter(q90_plus == TRUE, feature == "diff") %>%
  dplyr::select(-value, -feature, -q90) %>%
  pivot_wider(names_from = stat, values_from = q90_plus ) %>%
  mutate(n_ews = sum(kur,ac1,std,skw,fd, na.rm = TRUE)) %>%
  filter(n_ews > 0)


df_ews %>%
  #ggplot(aes(lon, lat)) + geom_tile(aes(fill = n_ews))
  ggplot(aes(biome)) + geom_bar() + coord_flip()

## calculate the bar plots also as proportion of the biome showing EWS.
# df_affected <- df_ews %>%
#   group_by(biome) %>%
#   summarise(affected_area = n())
#
# df_affected %>%
#   left_join((df_biomes %>% group_by(biome) %>% summarize(n = n()))) %>%
#   mutate(proportion = affected_area / n) %>%
#   filter(!is.na(biome)) %>%
#   ggplot(aes(biome, proportion)) +
#   geom_col( alpha = 0.7) +
#   coord_flip() +
#   theme_light()

# ## save results:
save(df_ews, file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/220325_detected_gpp_log_4yr-window.RData")

##
# load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200901_detected_terrestrial_ecosystem_respiration.RData")
#
# lats <- df_ews %>% pull(lat) %>% unique() %>% sort()
#
# lats <- as.character(lats)
# path <- "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/ews_halfwindow_lai/"
#
# file <- paste(path, "lat_",lats[460], ".csv", sep = "")
#
# tic()
# df <- read_csv(
#   file = file,
#   col_types = cols(
#       time = col_datetime(),
#       lon = col_double(),
#       gpp = col_double(),
#       gpp_1d = col_double(),
#       ews_std = col_double(),
#       ews_ac1 = col_double(),
#       ews_kur = col_double(),
#       ews_skw = col_double(),
#       ews_fd = col_double()
#       )
#   )
# toc()
#
# lons <- df_ews %>%
#   filter(lat == as.numeric(lats[460])) %>%
#   pull(lon) %>%
#   unique()
#
# lon_tundra <- df_biomes %>%
#   filter(lat == lats[460], biome == "Tundra") %>%
#    pull(lon) %>% unique()
#
# df %>% filter(lon %in% lons) %>%
#   filter(lon %in% lon_tundra) %>%
#   # pull(gpp) %>% #forecast::ndiffs()
#   # urca::ur.kpss(., lags = "long", use.lag = 0) %>% slot("teststat")
#   # ggplot(aes(time, ews_fd)) +
#   # geom_line(size = 0.5) +
#   # geom_smooth()
#   ggplot(aes(gpp, ews_std)) + geom_density2d()
