library(tidyverse)
# library()

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200819_summary_lai.RData")

# run the 07-ecoregions script and merge with results data frame:
df <- left_join(df, df_biomes)

q75 <- df %>% group_by(biome, stat) %>%
  filter(feature == "diff") %>%
  summarise(q75 = quantile(value, prob = 0.75))

df <- left_join(df, q75) %>%
  mutate(q75_plus = value > q75)

df %>%
  filter(stat == "fd", feature == "diff") %>%
  filter(!is.na(biome)) %>%
  ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = value, alpha = q75_plus)) + # , alpha = q75_plus
  scale_fill_viridis_c(option = "C", name = "Difference") +
  scale_alpha_discrete(range = c(0.3,1), name = "Percentile", label = c("< 90%", "> 90%")) +
   theme_void()

## select the pixels where stark differences were detected:
## J200825: Note that fd is not working, the same for all pixels with min 0.
df_ews <- df %>%
  filter(q75_plus == TRUE, feature == "diff") %>%
  group_by(lon, lat, biome) %>% # ggplot(aes(biome)) + geom_bar() + coord_flip()
  summarise(n_ews = n())

df_ews %>%
  # ggplot(aes(lon, lat)) + geom_tile(aes(fill = n_ews))
  ggplot(aes(biome)) + geom_bar() + coord_flip()

## calculate the bar plots also as proportion of the biome showing EWS.

df_affected <- df_ews %>%
  group_by(biome) %>%
  summarise(affected_area = n())

df_affected %>%
  left_join((df_biomes %>% group_by(biome) %>% summarize(n = n()))) %>%
  mutate(proportion = affected_area / n) %>%
  filter(!is.na(biome)) %>%
  ggplot(aes(biome, proportion)) +
  geom_col( alpha = 0.7) +
  coord_flip() +
  theme_light()

## save results:
save(df_ews, file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200825_detected_gpp.RData")

##
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200825_detected_gpp.RData")

lats <- df_ews %>% pull(lat) %>% unique() %>% sort()

lats <- as.character(lats)
path <- "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/ews_halfwindow_gpp/"
file <- paste(path, "lat_",lats[460], ".csv", sep = "")

tic()
df <- read_csv(
  file = file,
  col_types = cols(
      time = col_datetime(),
      lon = col_double(),
      gpp = col_double(),
      gpp_1d = col_double(),
      ews_std = col_double(),
      ews_ac1 = col_double(),
      ews_kur = col_double(),
      ews_skw = col_double(),
      ews_fd = col_double()
      )
  )
toc()

lons <- df_ews %>%
  filter(lat == as.numeric(lats[460])) %>%
  pull(lon) %>%
  unique()

lon_tundra <- df_biomes %>%
  filter(lat == lats[460], biome == "Tundra") %>%
   pull(lon) %>% unique()

df %>% filter(lon %in% lons) %>%
  filter(lon %in% lon_tundra) %>%
  # pull(gpp) %>% #forecast::ndiffs()
  # urca::ur.kpss(., lags = "long", use.lag = 0) %>% slot("teststat")
  # ggplot(aes(time, ews_fd)) +
  # geom_line(size = 0.5) +
  # geom_smooth()
  ggplot(aes(gpp, ews_std)) + geom_density2d()
