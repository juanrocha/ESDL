#### load tools ####
library(tidyverse)
library(tictoc)
library(rworldmap)
library(sf)
library(patchwork)
library(tidymodels)
library(scico)
library("rnaturalearth")
library("rnaturalearthdata")
# library(cleangeo)
data("coastsCoarse")

# load results:
# load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201029_summary_terrestrial_ecosystem_respiration_log.RData')

## newer results:
# new analysis with deltas: deltas is a df with the delta and abruptness per statistic. 19Mb

### GPP
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_gpp_log.RData") #19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_GPP_log.RData")

### TER
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_terrestrial_ecosystem_respiration_log.RData") #19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_TER_log.RData")


### ClorA
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_chlorA_log.RData") #19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData")

# dataset with biomes, ecosystems and countries:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/tererestrial_ecosystems.RData")
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/countries.RData")

ls()

## mapping layer:
# continents <- coastsCoarse %>%
#   st_as_sf()
# ggplot() + geom_sf(data = continents) + coord_sf()
# coastsCoarse %>% ggplot() + geom_path(aes(long,lat, group = group))


## combine the two data frames:
dat <- df_delta_detected %>%
  left_join(deltas %>% select(-starts_with("delta_")))

dat <- dat %>%
  mutate(detected = (ews_delta_ac1|ews_delta_std|ews_delta_skw|ews_delta_kur|ews_delta_fd)) %>%
  mutate(biome2 = fct_explicit_na(biome))

rm(deltas, df_delta_detected)

## for writing:
tt <- dat$detected %>% table()

all_perc <- tt[["TRUE"]] / (tt[["TRUE"]] + tt[["FALSE"]])

## Examples:
dat %>%
  ggplot(aes(lon,lat)) +
  geom_tile(aes(fill = value_delta_ac1, alpha = ews_delta_ac1)) +
  scale_fill_viridis_c(option = "C", name = expression(Delta)) +
  scale_alpha_discrete(name = "Detected") +
  #geom_path(aes(long,lat, group = group), size = 0.1,
  #  data = coastsCoarse) +
  guides(
    fill = guide_colourbar(
      title.position = "top",barwidth = 3, barheight = 0.3, order = 2),
    alpha = guide_legend(
      title.position = "top", keywidth = 0.3, keyheight = 0.3, order = 1)) +
  theme_void(base_size = 6) + labs(tag = "A") +
  #ylim(c(-60,NA)) +
  theme(
    legend.position = "bottom", #c(0.7,0.1),
    legend.direction = "horizontal",
    legend.box = "horizontal", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  )

base_map <- ne_countries(scale = "medium", returnclass = "sf")



#### Figure 1 & 2####

df_affected <- dat %>%
  filter(detected == TRUE) %>%
  group_by(biome) %>%
  summarise(affected_area = n())

df_affected <- df_affected %>%
  left_join((dat %>% group_by(biome) %>% summarize(n = n())))
  #when marine:
  #left_join((df_marine %>% group_by(biome) %>% count(.drop = TRUE)))

g2 <- df_affected %>%
  mutate(proportion = affected_area / n) %>%
  ggplot(aes(fct_rev(biome), proportion)) +
  geom_col(aes(fill = biome), alpha = 0.75, show.legend = FALSE) +
  labs(tag = "C", y = "Proportion of area", x = "Biomes") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_scico_d( palette = "romaO", na.value = "grey50") +
  coord_flip() +
  theme_light(base_size = 5) + theme(axis.text.y = element_blank())

g4 <- df_affected %>% 
  mutate(not_detected_area = n - affected_area,
         Detected = affected_area / sum(df_affected$n),
         `Not detected` = not_detected_area / sum(df_affected$n)) %>% 
  select(biome, Detected, `Not detected`) %>% 
  pivot_longer(2:3, names_to = "detection", values_to = "class") %>% 
  mutate(detection = as_factor(detection) %>% 
           fct_rev()) %>% 
  ggplot(aes(detection, class)) +
  geom_col(aes(fill = biome, alpha = detection), position = "stack", 
           show.legend = FALSE) +
  annotate("text", x = 2, y = round(all_perc + 0.02, digits = 2), hjust = 0, 
           label = paste(round(all_perc,2)*100, "%"  ), size = 2.5)+
  coord_flip() +
  scale_alpha_discrete("Detected", range = c(0.5,1)) +
  scale_fill_scico_d(palette = "romaO", na.value = "grey50") +
  labs(x = "Resilience loss", y = "Global aggregate", tag = "B") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal(base_size = 5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g3 <- dat %>%
  filter(n_ews > 0) %>%
  mutate(n_ews = as_factor(n_ews),
         n_ews = fct_relevel(n_ews, "5", after = 4)) %>% 
  ggplot(aes(fct_rev(biome))) +
  geom_bar(aes(fill = biome, alpha = n_ews)) +
  guides(fill = "none", alpha = guide_legend(
    title.position = "top", keywidth = 0.25, keyheight = 0.3, label.position = "bottom")) +
  coord_flip() +
  scale_alpha_discrete("Signals", range = c(0.5,1)) +
  scale_fill_scico_d(palette = "romaO", na.value = "grey50") +
  #scale_y_log10() +
  labs(tag = "D", y = "Area in pixels", x = "Biomes") +
  theme_light(base_size = 5) +
  theme(
    legend.position = c(0.80,0.25),
    legend.direction = "horizontal",
    legend.box = "horizontal", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 5))


quartz(width = 7, height = 4, pointsize = 6)

g1 <- dat %>%
  ggplot(aes(lon,lat)) +
  geom_tile(aes(fill = biome, alpha = detected)) +
  geom_path(
    data = map_data("world") %>% rename(lon = long),
    aes(map_id = region, group = group), size = 0.15 ) +
  scale_alpha_discrete(
    name = "Signal", labels = c("Not detected", "Detected")) +
  scale_fill_scico_d(
    name = "Terrestrial biomes", palette = "romaO", na.value = "grey50") +
  guides(
    fill = guide_legend(ncol = 4, title.position = "top"),
    alpha = guide_legend(ncol = 1, title.position = "top")
  ) + 
  theme_void(base_size = 6) + labs(tag = "A") + ylim(-62,NA) +
  theme(
    legend.position = "bottom", #c(0.7,0.1),
    legend.direction = "horizontal",
    legend.box = "horizontal", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 5, b = 2, l = 5, unit = "pt"),
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 5)
  ) +
  annotation_custom(
    grob = ggplotGrob(g2),
    xmin = -189, ymin = -20, xmax = -110, ymax = 20)+
  annotation_custom(
    grob = ggplotGrob(g3),
    xmin = -189, ymin = -60, xmax = -110, ymax = -20)+
  annotation_custom(
    grob = ggplotGrob(g4),
    xmin = -189, ymin = 20, xmax = -120, ymax = 50)

g1


ggsave(
  plot = g1,
  filename = "fig_detection_TER.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 7, height = 4, dpi = 400,
  bg = "white"
)


#### Figure 3: Marine realms ####

df_affected <- dat %>%
  #filter(detected == TRUE) %>%
  group_by(biome, detected) %>%
  summarise(affected_area = n()) %>% 
  pivot_wider(names_from = detected, names_prefix = "area_", values_from = "affected_area") %>% 
  rename(n = area_FALSE, affected_area = area_TRUE)


pal <- "hawaii"
g2 <- df_affected %>%
  mutate(proportion = affected_area / n) %>%
  ggplot(aes(fct_rev(biome), proportion)) +
  geom_col(aes(fill = biome), alpha = 0.75, show.legend = FALSE) +
  labs(y = "Proportion of area", x = "Marine realms") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_scico_d( palette = pal, na.value = "grey50") +
  coord_flip() + labs(tag = "B") +
  theme_light(base_size = 5) #+ theme(axis.text.y = element_blank())

g3 <- dat %>%
  filter(n_ews > 0) %>%
  mutate(n_ews = as_factor(n_ews),
         n_ews = fct_relevel(n_ews, "1","2","3","4","5")) %>%
  ggplot(aes(fct_rev(biome))) +
  geom_bar(aes(fill = biome, alpha = n_ews)) +
  guides(fill = "none", alpha = guide_legend(
    title.position = "top", keywidth = 0.25, keyheight = 0.3, label.position = "right")) +
  coord_flip() +
  scale_alpha_discrete("Signals",range = c(0.5,1)) +
  scale_fill_scico_d( palette = pal, na.value = "grey50") +
  labs(y = "Area in pixels", x = "Marine realms", tag = "C") +
  theme_light(base_size = 5) + 
  theme(
    legend.position = c(0.7, 0.4),
    #legend.direction = "vertical",
    #legend.box = "vertical", legend.title.align = 0.5,
    #legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 5)) 

g4 <- df_affected %>% 
  mutate(not_detected_area = n - affected_area,
         Detected = affected_area / nrow(dat),
         `Not detected` = not_detected_area / nrow(dat)) %>% 
  select(biome, Detected, `Not detected`) %>% 
  pivot_longer(2:3, names_to = "detection", values_to = "class") %>% 
  mutate(detection = as_factor(detection) %>% 
           fct_rev()) %>% 
  ggplot(aes(detection, class)) +
  geom_col(aes(fill = biome, alpha = detection), position = "stack", 
           show.legend = FALSE) +
  annotate("text", x = 2, y = round(all_perc + 0.02, digits = 2), hjust = 0, 
           label = paste(round(all_perc,2)*100, "%"  ), size = 2.5)+
  coord_flip() +
  scale_alpha_discrete("Detected", range = c(0.5,1)) +
  scale_fill_scico_d(palette = pal, na.value = "grey50") +
  labs(x = "Resilience loss", y = "Global aggregate", tag = "D") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_light(base_size = 5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g0 <- dat %>%
  ggplot(aes(lon,lat)) +
  geom_tile(aes(fill = biome, alpha = detected)) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  scale_alpha_discrete(
    name = "Signals of\n resilience loss", 
    label = c("Not detected", "Detected")) +
  scale_fill_scico_d(
    name = "Marine realms", palette = pal, na.value = "grey50") +
  guides(
    fill = "none",
    alpha = guide_legend(ncol = 1, title.position = "top")
  ) +
  theme_void(base_size = 6) + labs(tag = "A") + 
  ylim(c(-60,NA)) +
  theme(
    legend.position = c(0.1,0.95),
    legend.direction = "vertical",
    legend.box = "vertical", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 5, b = 2, l = 5, unit = "pt"),
    legend.key.size = unit(0.15, "cm"),
    legend.text = element_text(size = 6)
  ) +
  annotation_custom(
    grob = ggplotGrob(g3),
    xmin = 10, ymin = 52, xmax = 90, ymax = 90) +
  annotation_custom(
    grob = ggplotGrob(g4),
    xmin = 90, ymin = 52, xmax = 180, ymax = 90)+
  annotation_custom(
    grob = ggplotGrob(g2),
    xmin = -100, ymin = 52, xmax = 10, ymax = 90)


#quartz(width = 7, height = 4)

#g2 + g3 + plot_layout(widths = c(1,2))
g0

ggsave(
  plot = last_plot(),
  filename = "fig_detection_marine.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 7, height = 4, dpi = 400
)

#### Countries & ecosystems ####
dat <- dat %>% 
  left_join(df_eco) %>% 
  left_join(df_countries)

df_fig4 <- dat %>% 
  rename(country = name) %>% 
  filter(country != "Ashmore and Cartier Is.") %>%  #error with country code
  group_by(country) %>% 
  summarize(area = n(), detected_area = sum(detected), eco_types = unique(ecosystem)) %>% 
  filter(!is.na(country), !is.na(eco_types)) %>% 
  add_tally(name = "n_eco") %>% select(-eco_types) %>% 
  unique() %>% 
  mutate(prop = detected_area / area) %>% 
  arrange(desc(detected_area))

## checking missing values: coastal areas where the rasterize fails.
dat %>% 
  filter(is.na(name)) %>% ggplot(aes(lon,lat)) + geom_tile(aes(fill=detected))

g1 <- df_fig4 %>% 
  arrange(desc(detected_area)) %>% 
  mutate(country = as.character(country)) %>% ungroup() %>% 
  top_n(10, detected_area) %>% select(-area, -n_eco) %>% 
  mutate(country = as_factor(country), 
         country = fct_reorder(country, detected_area, sort)) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "stat", values_to = "value") %>% 
  mutate(stat = case_when(stat == "detected_area" ~ "Detected area (pixels)",
                          stat == "prop" ~ "Proportion of territory")) %>% 
  ggplot(aes(country, value)) + 
  geom_col() +
  facet_wrap(~ stat, scales = "free_x") +
  coord_flip() +
  labs(y = "Value", x = "Country", tag = "A") +
  theme_light(base_size = 7)

g2 <- df_fig4 %>% 
  arrange(desc(n_eco)) %>% 
  mutate(country = as.character(country)) %>% ungroup() %>% 
  top_n(10, n_eco) %>% select(-area, -detected_area) %>% 
  mutate(country = as_factor(country), 
         country = fct_reorder(country, n_eco, sort)) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "stat", values_to = "value") %>% 
  mutate(stat = case_when(stat == "n_eco" ~ "Ecosystems lossing resilience",
                          stat == "prop" ~ "Proportion of territory")) %>% 
  ggplot(aes(country, value)) + 
  geom_col() +
  facet_wrap(~ stat, scales = "free_x") +
  coord_flip() +
  labs(y = "Value", x = "Country", tag = "B") +
  theme_light(base_size = 7) 

## for writing
df_fig4 %>% 
  arrange(desc(n_eco)) %>% 
  mutate(country = as.character(country)) %>% ungroup() %>% 
  top_n(10, prop) %>%  arrange(desc(prop))

g1 + g2

ggsave(
  plot = last_plot(),
  filename = "fig_countries.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 6, height = 3, dpi = 320
)
#### Time coherence ####

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201028_segmented_terrestrial_ecosystem_respiration_log.RData")
px_ter <- px_results
px_ter$var <- "Terrestrial Ecosystem respiration"

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_gpp_log.RData")
px_gpp <- px_results
px_gpp$var <- "Gross primary productivity"

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_chlorA_log.RData")
px_clo <- px_results
px_clo$var <- "Chlorophyll-A"

rm(px_results)

px_all <- bind_rows(px_clo, px_gpp, px_ter)

names(px_all)

px_all %>% 
  mutate(
    stat = case_when(
      stat == "ac1" ~ "Autocorrelation lag-1",
      stat == "std" ~ "Standard deviation",
      stat == "kur" ~ "Kurtosis",
      stat == "skw" ~ "Skewness",
      stat == "fd" ~ "Fractal dimension"
    )) %>% 
  ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = time)) +
  geom_path(aes(long,lat, group = group), size = 0.1, data = coastsCoarse) +
  facet_grid(stat ~ var) +
  #scale_fill_datetime(name = "Year", low = "dodgerblue", high = "red") +
  scale_fill_scico(name = "Year", palette = "hawaii", trans = "time") +
  ylim(-60,NA) + labs(x = "", y ="") +
  theme_light(base_size = 7) + 
  theme(legend.position = "bottom", 
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(15, "mm"), 
        axis.text = element_blank())

ggsave(
  plot = last_plot(),
  filename = "sm_temporal_map.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 6, height = 5, dpi = 400
)


g1 <- px_all %>% 
  mutate(
    stat = case_when(
      stat == "ac1" ~ "Autocorrelation lag-1",
      stat == "std" ~ "Standard deviation",
      stat == "kur" ~ "Kurtosis",
      stat == "skw" ~ "Skewness",
      stat == "fd" ~ "Fractal dimension"
    )) %>% 
  ggplot(aes(time, lon)) +
  stat_density2d_filled(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
  facet_grid(stat~var) + 
  scale_fill_viridis_c(option = "D") + 
  labs(tag = "A", x = "Time", y = "Longitud") +
  theme_light(base_size = 6) + 
  theme(legend.key.height = unit(10, "mm"),
        legend.key.width = unit(3, "mm"))

g3 <- px_all %>% 
  mutate(
    stat = case_when(
      stat == "ac1" ~ "Autocorrelation lag-1",
      stat == "std" ~ "Standard deviation",
      stat == "kur" ~ "Kurtosis",
      stat == "skw" ~ "Skewness",
      stat == "fd" ~ "Fractal dimension"
    )) %>% 
  ggplot(aes(time, lat)) +
  stat_density2d_filled(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
  facet_grid(stat~var) + 
  scale_fill_viridis_c(option = "D") + labs(tag = "B", x = "Time", y = "Latitude") +
  theme_light(base_size = 6) +
  theme(legend.key.height = unit(10, "mm"),
      legend.key.width = unit(3, "mm"))

ggsave(
  plot = (g1/g3), # g1 is lat, g3 is lon
  filename = "sm_temporal_clustering.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 6, height = 8, dpi = 320
)

px_all %>% 
  select(var, lon, lat, stat, time) %>% 
  mutate(
    stat = case_when(
      stat == "ac1" ~ "Autocorrelation lag-1",
      stat == "std" ~ "Standard deviation",
      stat == "kur" ~ "Kurtosis",
      stat == "skw" ~ "Skewness",
      stat == "fd" ~ "Fractal dimension"
    ),
    var = case_when(
      var == "Chlorophyll-A" ~ "ClorA",
      var == "Terrestrial Ecosystem respiration" ~ "TER",
      var == "Gross primary productivity" ~ "GPP"
    )
  ) %>% 
  pivot_wider(names_from = stat, values_from = time) %>% 
  GGally::ggpairs(
    columns = 4:8, aes(fill = var, color = var),
    lower = list(continuous = GGally::wrap("points", alpha = 0.05, size = 0.1)),
    diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.2)),
    upper = list(continuous = GGally::wrap("cor", size = 2)))  + 
  theme_light(base_size = 5)

ggsave(
  plot = last_plot(),
  filename = "sm_temporal_correlogram.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 5, height = 4, dpi = 320
)



px_all %>% 
  select(var, lon, lat, stat, time) %>% 
  pivot_wider(names_from = var, values_from = time) %>% 
  GGally::ggpairs(
    columns = 4:6, aes(fill = stat, color = stat),
    lower = list(continuous = GGally::wrap("points", alpha = 0.05)),
    diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.2)))

#### Regression results ####
## Terrestrial ecosystems


# load data: Run first GPP, then TER, and then combine them.
load("Results/regression_data_GPP.RData")
load("Results/regression_data_TER.RData")

load("Results/210529_rf_GPP.RData")
load("Results/210526_rf_TER.RData")



deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
  mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
  mutate(detected = as_factor(detected),
         detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
  mutate(biome = fct_explicit_na(biome))

## Data split
data_split <- initial_split(
  deltas %>% filter(n_ews == 0 | n_ews >1),
  strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)

## load the recipe
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

rf_prep <- prep(rf_rcp)
juiced <- juice(rf_prep)

## model specification
rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("classification") %>%
  set_engine("ranger") 

rf_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(cols = min_n:mtry, names_to = "tune_parm", values_to = "value") %>% 
  ggplot(aes(value, mean)) + 
  geom_point(aes(color = tune_parm)) +
  facet_wrap(~tune_parm, scales = "free_x")

regular_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line() +
  geom_point()

final_fit %>% 
  collect_predictions() %>% 
  mutate(correct = case_when(
    detected == .pred_class ~ "correct", TRUE ~ "incorrect"
  )) %>% 
  bind_cols(test_data) %>% 
  ggplot(aes(lon, lat, fill  = correct)) +
  geom_tile() +
  theme_void()

best_mod <- select_best(regular_res)

regular_res %>% collect_metrics() %>% arrange(desc(mean))

rf_final <- finalize_model(
  rf_model,
  best_mod
)

rf_workflow <- workflow() %>%
  add_recipe(rf_rcp) %>%
  add_model(rf_model) 

## extracting the dataset for plotting.
tic()
df_rf2 <- rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(detected ~ .,
      data = juice(rf_prep)) 
toc() # ~ 5mins

## extract p_values
## ranger::importance_pvalues(df_rf2$fit, method = "altmann", formula = df_rf2$preproc$terms, data = juice(rf_prep))

df_gpp <- tibble(
  variable = names(df_rf1$fit$variable.importance),
  importance = df_rf1$fit$variable.importance, 
  response = "Gross Primary Productivity")


df_ter <- tibble(
  variable = names(df_rf2$fit$variable.importance),
  importance = df_rf2$fit$variable.importance, 
  response = "Terrestrial Ecosystem Respiration")

g2 <- bind_rows(df_gpp, df_ter) %>% 
  rename(term = variable) %>%
  mutate(type = case_when(
    str_detect(term, "temp_|prec_|burn_") ~ "Climate",
    str_detect(term, "lcc_|prop_change") ~ "Land cover",
    str_detect(term, "biome") ~ "Biomes",
    str_detect(term, "Intercept|lon|lat") ~ ".")) %>% 
  mutate(term = str_remove_all(term, "biome_"), 
         term = str_replace(term, "lcc_80", "Needleleaved deciduous closed to open trees"),
         term = str_replace(term, "lcc_71", "Needleleaved evergreen closed to open trees (100-40%)"),
         term = str_replace(term, "lcc_61", "Broadleaved evergreen closed to open trees (100-40%)"),
         term = str_replace(term, "lcc_40", "Natural and semi-natural primarily terrestrial vegetation"),
         term = str_replace(term, "lcc_30", "Cultivated and managed terrestrial areas"),
         term = str_replace(term, "lcc_202", "Unconsolidated bare areas"),
         term = str_replace(term, "lcc_200", "Bare areas"),
         term = str_replace(term, "lcc_190", "Urban areas"),
         term = str_replace(term, "lcc_130", "Grassland"),
         term = str_replace(term, "lcc_11", "Rainfed herbaceous crops"),
         term = str_replace(term, "lcc_100", "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)"),
         term = str_replace(term, "lcc_10", "Rainfed shrub crops, tree crops, herbaceous crops"),
         term = str_replace(term, "X.Missing", "Missing value"), 
         term = str_replace(term, "temp_std_longterm", "Temperature: long term variation (std)"),
         term = str_replace(term, "temp_std_fast_oscillations", "Temperature: fast oscillations (std)"),
         term = str_replace(term, "temp_std_annual_cycle", "Temperature: annual cycle (std)"),
         term = str_replace(term, "temp_slope", "Temperature: slope of linear trend"),
         term = str_replace(term, "temp_mean", "Temperature: mean"),
         term = str_replace(term, "prec_std_longterm", "Precipitation: long term variation (std)"),
         term = str_replace(term, "prec_std_fast_oscillations", "Precipitation: fast oscillations (std)"),
         term = str_replace(term, "prec_std_annual_cycle", "Precipitation: annual cycle (std)"),
         term = str_replace(term, "prec_slope", "Precipitation: slope of linear trend"),
         term = str_replace(term, "prec_mean", "Precipitation: mean"),
         term = str_replace(term, "prop_change", "Proportion of land cover change"),
         term = str_replace(term, "lon$", "Longitud"),
         term = str_replace(term, "lat$", "Latitude"),
         term = str_replace_all(term, "\\.", " "),
         term = str_replace_all(term, "_", " "), 
         term = str_trim(term, "both"), 
         term = str_replace_all(term, "  ", " "),
         term = str_to_sentence(term)
  ) %>% 
  group_by(response) %>% 
  top_n(10, importance) %>% 
  mutate(term = as_factor(term) %>% fct_reorder(., importance, mean)) %>% 
  ggplot(aes(importance, term)) +
  geom_col() + 
  facet_wrap(response~., scales = "free", nrow = 2) +
  labs(x = "Importance (permutation)",
       y = "Random forest strongest predictors", tag = "B") +
  theme_light(base_size = 6) +
  theme(legend.position = "top") 

# g1 + g2 + plot_layout(widths = c(2,1))
# 
# ggsave(filename = "figures/fig_regressions_terrestrial.png", plot = last_plot(),
#        device = "png", dpi = 300,
#        width = 7, height = 4, units = "in")

### Logistic regression:
## Workflow: run the code for GPP first, then for TER, combine the datasets and produce one summarizing figure

log_rcp <- recipe(
  detected ~ temp_std_longterm + temp_std_annual_cycle + temp_std_fast_oscillations + 
    prec_std_longterm + prec_std_annual_cycle + prec_std_fast_oscillations + temp_slope + 
    prec_slope + temp_mean + prec_mean + prop_change  +  lcc_10 + lcc_11 + lcc_30 + 
    lcc_40 + lcc_61 +  lcc_71 +  lcc_80 +  lcc_100 +  lcc_130 +  lcc_190 + lcc_200 + lcc_202 +
    burn_area + biome + lon + lat,
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

## Interpretation note: In the help(step_downsample) is explained that the downsampling is meant to be done only during training. 

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
toc() # 62sec


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
# 
# Notes for writing: roc_auc 0.714, accuracy 0.648 for GPP, 0.655 and 0.617 for TER

log_fit$fit$fit

# use df2 or df3 depending if GPP or TER respectively
reg_df3 <- pull_workflow_fit(log_fit)  %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) 

log_auc <- log_res %>%
  collect_predictions() %>%
  roc_curve(detected, .pred_TRUE) %>%
  autoplot()

# once you've run the regression twice, merge the two datasets
df_reg <- bind_rows(
  reg_df2 %>% mutate(response = "Gross Primary Productivity"),
  reg_df3 %>% mutate(response = "Terrestrial Ecosystem Respiration")
)

g1 <- df_reg %>%
  mutate(p_value = ifelse(p.value > 0.05, "p>0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%  #pull(p_value) %>% unique()
  mutate(p_value = as_factor(p_value) %>% 
         fct_relevel(., "p<0.01", "p<0.05", "p>0.05")  ) %>%  
  mutate(type = case_when(
    str_detect(term, "temp_|prec_|burn_") ~ "Climate",
    str_detect(term, "lcc_|prop_change") ~ "Land cover",
    str_detect(term, "biome") ~ "Biomes",
    str_detect(term, "Intercept|lon|lat") ~ ".")) %>% 
  mutate(term = str_remove_all(term, "biome_"), 
         term = str_replace(term, "lcc_80", "Needleleaved deciduous closed to open trees"),
         term = str_replace(term, "lcc_71", "Needleleaved evergreen closed to open trees (100-40%)"),
         term = str_replace(term, "lcc_61", "Broadleaved evergreen closed to open trees (100-40%)"),
         term = str_replace(term, "lcc_40", "Natural and semi-natural primarily terrestrial vegetation"),
         term = str_replace(term, "lcc_30", "Cultivated and managed terrestrial areas"),
         term = str_replace(term, "lcc_202", "Unconsolidated bare areas"),
         term = str_replace(term, "lcc_200", "Bare areas"),
         term = str_replace(term, "lcc_190", "Urban areas"),
         term = str_replace(term, "lcc_130", "Grassland"),
         term = str_replace(term, "lcc_11", "Rainfed herbaceous crops"),
         term = str_replace(term, "lcc_100", "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)"),
         term = str_replace(term, "lcc_10", "Rainfed shrub crops, tree crops, herbaceous crops"),
         term = str_replace(term, "X.Missing", "Missing value"), 
         term = str_replace(term, "temp_std_longterm", "Temperature: long term variation (std)"),
         term = str_replace(term, "temp_std_fast_oscillations", "Temperature: fast oscillations (std)"),
         term = str_replace(term, "temp_std_annual_cycle", "Temperature: annual cycle (std)"),
         term = str_replace(term, "temp_slope", "Temperature: slope of linear trend"),
         term = str_replace(term, "temp_mean", "Temperature: mean"),
         term = str_replace(term, "prec_std_longterm", "Precipitation: long term variation (std)"),
         term = str_replace(term, "prec_std_fast_oscillations", "Precipitation: fast oscillations (std)"),
         term = str_replace(term, "prec_std_annual_cycle", "Precipitation: annual cycle (std)"),
         term = str_replace(term, "prec_slope", "Precipitation: slope of linear trend"),
         term = str_replace(term, "prec_mean", "Precipitation: mean"),
         term = str_replace(term, "prop_change", "Proportion of land cover change"),
         term = str_replace(term, "lon$", "Longitud"),
         term = str_replace(term, "lat$", "Latitude"),
         term = str_replace(term, "burn_area", "Burnt_area"),
         term = str_replace_all(term, "\\.", " "),
         term = str_replace_all(term, "_", " "), 
         term = str_trim(term, "both"), 
         term = str_replace_all(term, "  ", " "),
         term = str_to_sentence(term)
  ) %>% 
  ggplot(aes(estimate, term)) +
  geom_point(aes(color = p_value), size = 0.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, color = p_value), size = 0.25) +
  geom_vline(xintercept = 1, size = 0.3) + 
  facet_grid(type~response, scales = "free", space = "free_y", switch = "y") +
  scale_color_manual("", values = c("#6a3d9a", "#ff7f00", "#a6cee3")) +
  labs(x = "Odds of detecting symptoms of resilience loss",
       y = "Explanatory variables", tag = "A") +
  theme_light(base_size = 6) +
  theme(legend.position = "top") 

g1

# ggsave(filename = "figures/fig_regression_terrestrial.png", plot = g1,
#        device = "png", dpi = 300,
#        width = 4.5, height = 4, units = "in")

### Marine ecosystems
load("Results/210608_rf_chlorA.RData")
load("Results/regression_data_chlorA.RData")



## Logistic regression:
#### Data exploration to decide on variables ####
deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
  mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
  mutate(detected = as_factor(detected),
         detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
  mutate(biome = fct_explicit_na(biome))



## Data split
data_split <- initial_split(
  deltas %>% filter(n_ews == 0 | n_ews >1),
  strata = detected, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)


log_rcp <- recipe(
  detected ~ temp_std_longterm + temp_std_annual_cycle + 
    temp_std_fast_oscillations + temp_slope + temp_mean +
    sss_std_longterm + sss_std_annual_cycle + sss_std_fast_oscillations +  
    sss_slope +  sss_mean + biome + lon + lat,
  data = train_data) %>%
  step_dummy(biome) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>%
  #step_interact(terms = ~ sss_mean:temp_mean) %>%
  themis::step_downsample(detected)

log_model <- logistic_reg() %>%
  set_engine("glm", family = "binomial")

log_workflow <- workflow() %>%
  add_model(log_model) %>%
  add_recipe(log_rcp)

tic()
log_fit <- fit(log_workflow, train_data)
toc() #1.2s

tic()
reg_df <- pull_workflow_fit(log_fit) %>% 
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) 
toc() #26s


folds <- vfold_cv(train_data, v=10)

keep_pred <- control_resamples(save_pred = TRUE)

tic()
log_res <- log_workflow %>%
  fit_resamples(
    resamples = folds,
    control = keep_pred)
toc() #10s

collect_metrics(log_res) 

#Note for writing roc 0.599, acuracy 0.588

log_fit$fit$fit

log_auc <- log_res %>%
  collect_predictions() %>%
  roc_curve(detected, .pred_TRUE) %>%
  autoplot()

g3 <- reg_df %>%
  mutate(response = "Chlorophyll A") %>% 
  mutate(p_value = ifelse(p.value > 0.05, "p>0.05", ifelse(p.value <0.01, "p<0.01", "p<0.05"))) %>%  #pull(p_value) %>% unique()
  mutate(p_value = as_factor(p_value) %>% 
           fct_relevel(., "p<0.01", "p<0.05", "p>0.05")  ) %>%  
  mutate(type = case_when(
    str_detect(term, "temp_") ~ "Temperature",
    str_detect(term, "sss_") ~ "Salinity",
    str_detect(term, "biome") ~ "Marine realms",
    str_detect(term, "Intercept|lon|lat") ~ ".")) %>% 
  mutate(term = str_remove_all(term, "biome_"), 
         term = str_replace(term, "X.Missing", "High seas"), 
         term = str_replace(term, "temp_std_longterm", "Long term variation (std)"),
         term = str_replace(term, "temp_std_fast_oscillations", "Fast oscillations (std)"),
         term = str_replace(term, "temp_std_annual_cycle", "Annual cycle (std)"),
         term = str_replace(term, "temp_slope", "Slope of linear trend"),
         term = str_replace(term, "temp_mean", "Mean"),
         term = str_replace(term, "sss_std_longterm", "Long term variation (std)"),
         term = str_replace(term, "sss_std_fast_oscillations", "Fast oscillations (std)"),
         term = str_replace(term, "sss_std_annual_cycle", "Annual cycle (std)"),
         term = str_replace(term, "sss_slope", "Slope of linear trend"),
         term = str_replace(term, "sss_mean", "Mean"),
         term = str_replace(term, "lon$", "Longitud"),
         term = str_replace(term, "lat$", "Latitude"),
         term = str_replace_all(term, "\\.", " "),
         term = str_replace_all(term, "_", " "), 
         term = str_trim(term, "both"), 
         term = str_replace_all(term, "  ", " "),
         term = str_to_sentence(term)
  ) %>% 
  ggplot(aes(estimate, term)) +
  geom_point(aes(color = p_value), size = 0.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, color = p_value), size = 0.25) +
  geom_vline(xintercept = 1, size = 0.3) + 
  facet_grid(type~response, scales = "free", space = "free_y", switch = "y") +
  scale_color_manual("", values = c("#6a3d9a", "#ff7f00", "#a6cee3")) +
  labs(x = "Odds of detecting symptoms of resilience loss",
       y = "Explanatory variables", tag = "C") +
  theme_light(base_size = 6) +
  theme(legend.position = "top") 

g3


### random forest
#### random forest ####
rf_rcp <- recipe(
  detected ~ temp_std_longterm + temp_std_annual_cycle + 
    temp_std_fast_oscillations + temp_slope + temp_mean +
    sss_std_longterm + sss_std_annual_cycle + sss_std_fast_oscillations +  
    sss_slope +  sss_mean + biome ,
  data = train_data) %>%
  step_dummy(biome) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>%
  #step_interact(terms = ~ prec_var:temp_var) %>%
  themis::step_downsample(detected)


rf_prep <- prep(rf_rcp)
juiced <- juice(rf_prep)

## model specification
rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("classification") %>%
  set_engine("ranger") #importance = "impurity"

## workflow:
rf_workflow <- workflow() %>%
  add_recipe(rf_rcp) %>%
  add_model(rf_model) 

tic()
df_plot <- rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(detected ~ .,
      data = juice(rf_prep)) 
toc() #4.2m


df_plot %>% vip::vip(geom = "point", include = TRUE) +
  labs(title = "Random forest on detection", 
       subtitle = "ROC area under the curve 0.829") +
  theme_light(base_size = 8)

df_mar <- tibble(
  term = names(df_plot$fit$variable.importance),
  importance = df_plot$fit$variable.importance, 
  response = "Chlorophyll A")

g4 <- df_mar %>% 
  mutate(type = case_when(
    str_detect(term, "temp_") ~ "Temperature",
    str_detect(term, "sss_") ~ "Salinity",
    str_detect(term, "biome") ~ "Marine realms",
    str_detect(term, "Intercept|lon|lat") ~ ".")) %>% 
  mutate(term = str_remove_all(term, "biome_"), 
         term = str_replace(term, "X.Missing", "High seas"), 
         term = str_replace(term, "temp_std_longterm", "Temperature: Long term variation (std)"),
         term = str_replace(term, "temp_std_fast_oscillations", "Temperature: Fast oscillations (std)"),
         term = str_replace(term, "temp_std_annual_cycle", "Temperature: Annual cycle (std)"),
         term = str_replace(term, "temp_slope", "Temperature: Slope of linear trend"),
         term = str_replace(term, "temp_mean", "Temperature: Mean"),
         term = str_replace(term, "sss_std_longterm", "Salinity: Long term variation (std)"),
         term = str_replace(term, "sss_std_fast_oscillations", "Salinity: Fast oscillations (std)"),
         term = str_replace(term, "sss_std_annual_cycle", "Salinity: Annual cycle (std)"),
         term = str_replace(term, "sss_slope", "Salinity: Slope of linear trend"),
         term = str_replace(term, "sss_mean", "Salinity: Mean"),
         term = str_replace(term, "lon$", "Longitud"),
         term = str_replace(term, "lat$", "Latitude"),
         term = str_replace_all(term, "\\.", " "),
         term = str_replace_all(term, "_", " "), 
         term = str_trim(term, "both"), 
         term = str_replace_all(term, "  ", " "),
         term = str_to_sentence(term)
  )  %>% 
  top_n(15, importance) %>% 
  mutate(term = as_factor(term) %>% fct_reorder(., importance)) %>% 
  ggplot(aes(importance, term)) +
  geom_col() + 
  facet_wrap(response~., scales = "free", nrow = 2) +
  labs(x = "Importance (permutation)",
       y = "Random forest strongest predictors", tag = "D") +
  theme_light(base_size = 6) +
  theme(legend.position = "top") 

g3 + g4 + plot_layout(widths = c(2,1))

ggsave(filename = "figures/fig_regressions_marine.png", plot = last_plot(),
       device = "png", dpi = 300,
       width = 7, height = 3.5, units = "in")


## Version combined
g1 + g2 + g3 + g4 + 
  plot_layout(widths = c(2,1), heights = c(2,1), ncol =2, nrow=2)

ggsave(filename = "paper/figures/fig_regressions_combined.png", plot = last_plot(),
       device = "png", dpi = 400,
       width = 7, height = 7, units = "in")

#### Supplementary material ####


stat_ews <- c("Standard deviation", "Autocorrelation lag-1", "Skewness",
 "Kurtosis", "Fractal dimension")
vals <- names(dat)[names(dat) %>% str_detect("value_delta_.")][c(5,1,4,3,2)]
alphas <- names(dat)[names(dat) %>% str_detect("ews_delta_.")][c(5,1,4,3,2)]

plot_maps <- list()
tags <- LETTERS[1:5]

tic()
plot_maps <- pmap(
  .l = list(stat_ews, tags, vals, alphas),
  .f = function(stat_ews, tags, vals, alphas){
    dat %>%
      ggplot(aes(lon, lat)) +
      ## neat treak to call variables inside ggplot!
      #source: https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/
      geom_tile(aes(fill = .data[[vals]], alpha = .data[[alphas]])) +
      scale_fill_viridis_c(
        option = "C",name = expression(Delta)) +
      scale_alpha_discrete(
        range = c(0.2,1), name = "Percentile",
        label = c(">5% <95%", "<5% >95%")) +
      guides(
        fill = guide_colourbar(
          title.position = "top",barwidth = 4.5, barheight = 0.3, order = 2),
        alpha = guide_legend(
          title.position = "top", keywidth = 0.3, keyheight = 0.3, order = 1)) +
      theme_void(base_size = 6) + labs(tag = tags, title = stat_ews) +
      theme(
        legend.position = "bottom",#c(0.6,0.1),
        legend.direction = "horizontal",
        legend.box = "horizontal", legend.title.align = 0.5,
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        plot.title = element_text(hjust = 0.5)
      )  #+
      # annotation_custom(
      #   grob = ggplotGrob(
      #     ggplot(
      #       data = dat,
      #       aes(.data[[vals]], fct_rev(biome))) +
      #       geom_violin(aes(fill = biome, color = biome),
      #                    alpha = 0.5, show.legend = FALSE, size = 0.05
      #                  ) + #outlier.size = 0.1
      #       labs(y = "Biomes", x = expression(Delta)) +
      #       scale_fill_scico_d(palette = "romaO", na.value = "grey50") + 
      #       scale_color_scico_d(palette = "romaO", na.value = "grey50") +
      #       theme_light(base_size = 6) + theme(axis.text.y = element_blank())
      #   ), xmin = -185, ymin = -60, xmax = -90, ymax = 10)

  }
)
toc() # 5 secs

plot_maps[[6]] <-  dat %>%
  ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = n_ews, alpha = detected), show.legend = TRUE) +
  #geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  scale_alpha_discrete(range = c(0.25,1), name = "Detected") +
  scale_fill_viridis_c("Number of signals", option = "B") +
  labs(tag = "F", title = "Summary") + #ylim(-54.625, 79.875) +
  #xlim(-179.875,179.875) +
  guides(fill = guide_colourbar(
      title.position = "top",barwidth = 4.5, barheight = 0.2),
      alpha = guide_legend(
        title.position = "top", keywidth = 0.3, keyheight = 0.3, order = 1)) +
  theme_void(base_size = 6) +
  theme(
    legend.position = "bottom", # c(0.6,0.1), #terrestrial
    legend.direction = "horizontal",
    legend.box = "horizontal", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
  plot.title = element_text(hjust = 0.5))

quartz(width = 7.5, height = 8)

g4 <- wrap_plots(plot_maps) +
  plot_layout(nrow = 3, ncol = 2,
    widths = unit(c(90, 90), "mm"),
    heights = unit(c(5,5,5), "cm"))
g4

## correct measures for terrestrial width = 7.5, height = 7

ggsave(
  plot = g4,
  filename = "figS_marine.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 7.5, height = 8, , dpi = 400
)





g1 <- dat %>%
  ggplot(aes(abs(value_delta_std), fct_rev(biome))) +
  geom_boxplot(aes(fill = biome), alpha = 0.5, show.legend = FALSE,
    size = 0.1, outlier.size = 0.1) +
  labs(tag = "A", x = "Absolute difference in standard deviation", y = "Biomes") +
  # geom_vline(
  #     xintercept = quants, size = 0.25, linetype = 2, color = "red") +
  # annotate(
  #     "text", x = quants, y = 13.8, #y = 18.2, # terrestrial
  #     label = c(" 50%"," 75%", " 90%", " 95%"),
  #     hjust = 0, size = 1.5)+
  geom_rug(aes(color = abs(value_delta_std)), size = 0.15, show.legend = FALSE,
    sides = "b", outside = FALSE) +
  scale_color_viridis_c(option = "C") +
  scale_y_discrete(expand = expansion(mult = c(0.1,0.1))) +
  coord_cartesian(clip = "off") +
  theme_light(base_size = 6)
  # theme(axis.text.y = element_blank())
## calculate the bar plots also as proportion of the biome showing EWS.




#### temporal coherence ####
## I need to repeat the segmented regressiions to pull out the temporal coherence graph.
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_gpp_log.RData')
px_gpp <- px_results
px_gpp$variable <- "Gross primary productivity"
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_chlorA_log.RData')
px_clo <- px_results
px_clo$variable <- "Chlorophyll-A"
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201028_segmented_terrestrial_ecosystem_respiration_log.RData')
px_ter <- px_results
px_ter$variable <- "Terrestrial ecosystem respiration"

px_results <- bind_rows(px_ter, px_clo, px_gpp)

px_results %>%
  ggplot(aes(lon,lat)) +
  geom_tile(aes(fill = time)) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  facet_grid(stat~variable) +
  scale_fill_viridis_c() +
  theme_light(base_size = 6)



#### for writing ####


#### Permutation tests ####
load("Results/perm_results_chlorA_211101.RData")
prm_clo <- test_all |> 
    mutate(variable = "Chlorophyll A")
load("Results/perm_results_GPP_211101.RData")
prm_gpp <- test_all |> 
    mutate(variable = "Gross Primary Productivity")
load("Results/perm_results_TER_211101.RData")
prm_ter <- test_all |> 
    mutate(variable = "Terrestrial Ecosystem Resipiration")

prm_df <- bind_rows(prm_clo, prm_gpp, prm_ter) 

plt <- prm_df |>
    mutate(ews = case_when(
        ews == "ews_ac1" ~ "Autocorrelation lag-1",
        ews == "ews_fd" ~ "Fractal dimension",
        ews == "ews_kur" ~ "Kurtosis",
        ews == "ews_skw" ~ "Skewness",
        ews == "ews_std" ~ "Standard deviation"
    )) |> 
    mutate(ews = as_factor(ews), 
           ews = fct_relevel(ews, "Standard deviation"),
           ews = fct_relevel(ews, "Fractal dimension", after = Inf)) |> 
    ggplot(aes(abs_delta, real)) +
    geom_boxplot(aes(color = real, fill = real), alpha = 0.25, 
                 show.legend = TRUE, size = 0.25, outlier.size = 0.15) +
    facet_wrap(variable ~ ews, scales = "free_x", nrow = 3) +
    scale_fill_brewer("Group", palette = "Set1", labels = c("Permutations", "Real")) +
    scale_color_brewer("Group", palette = "Set1", labels = c("Permutations", "Real")) +
    labs(y = "", x = expression(paste("|", Delta, "|"))) +
    theme_light(base_size = 6) +
    theme(axis.text.y = element_blank(), legend.position = "top")
plt
#ggpval::add_pval(plt, pairs = list(c(1,2)), test = 'wilcox.test')

ggsave(
    plot = plt,
    filename = "figS_permutations2.png",
    path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
    device = "png",
    width = 5.5, height = 3, dpi = 500
)

## for writing: all text show that the mean between real and permutations is not the same, the difference is not equal to zero, all p-values < 0.05
prm_df |> 
    split(~ews*variable) |>
    map(~wilcox.test(abs_delta ~ real, data = .))

#### MARSS results ####
# create a function to recover the summary 
extract_summary <- function(x){# x is the output dataframe from the lambda analysis
    y <- x |>
        mutate(col = case_when(
            lamda-lamda.se > 1 ~ "L2",
            lamda > 1 & lamda-lamda.se <1 ~ "L1",
            TRUE ~ "L0"
        )) |>
        group_by(group) |>
        summarize(
            lamda_cats = case_when(
                any(col == "L2") ~ "L2",
                any(col == "L1") & all(col != "L2") ~ "L1",
                all(col == "L0") ~ "L0"
            ),
            lamda_points = sum(lamda>1, na.rm = TRUE),
            max_lamda = max(lamda, na.rm = TRUE)) #|> 
        # arrange(desc(lamda_points)) |>
        # print(n = 100)
    return(y)
}



load("Results/211108_lambda_chlorA_detected.RData")
chlo1 <- extract_summary(out) |> 
    add_column(variable = "Chlorophyll A", detected = TRUE)
load("Results/211108_lambda_chlorA_non-detected.RData")
chlo2 <- extract_summary(out) |> 
    add_column(variable = "Chlorophyll A", detected = FALSE)
load("Results/211108_lambda_gpp_detected.RData")
gpp1 <- extract_summary(out) |> 
    add_column(variable = "Gross Primary\n Productivity", detected = TRUE)
load("Results/211108_lambda_gpp_non-detected.RData")
gpp2 <- extract_summary(out) |> 
    add_column(variable = "Gross Primary\n Productivity", detected = FALSE) 
load("Results/211108_lambda_ter_detected.RData")
ter1 <- extract_summary(out) |> 
    add_column(variable = "Terrestrial Ecosystem\n Respiration", detected = TRUE)
load("Results/211108_lambda_ter_non-detected.RData")
ter2 <- extract_summary(out) |> 
    add_column(variable = "Terrestrial Ecosystem\n Respiration", detected = FALSE)

df_all <- bind_rows(chlo1, chlo2, gpp1, gpp2, ter1, ter2)
rm(chlo1, chlo2, gpp1, gpp2, ter1, ter2, out)

a <- df_all |> 
    ggplot(aes(detected)) +
    geom_bar(aes(fill = lamda_cats), position = "stack") +
    facet_wrap(~ variable) +
    scale_fill_brewer(
        expression(lambda), palette = "Set2", 
        labels = expression(lambda < 1, lambda > 1,  lambda - s.e >1)) +
    labs(x = "Detected", y = "Number of pixels tested", tag = "A") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.86, 0.8), legend.key.size = unit(0.25, "cm"),
          legend.background = element_rect(fill = alpha("white", 0.5)))

b <- df_all |> 
    ggplot(aes(detected, max_lamda)) +
    geom_boxplot(color = "grey50", fill = "gray50", alpha = 0.5) +
    geom_hline(yintercept = 1, color = "orange", linetype = 2) +
    facet_wrap(~variable) +
    labs(x = "Detected", y = expression(lambda[Max]), tag = "B") +
    theme_light(base_size = 6)

c <- df_all |> 
    ggplot(aes(detected, lamda_points)) +
    geom_boxplot(color = "grey50", fill = "gray50", alpha = 0.5) +
    facet_wrap(~variable)+
    labs(x = "Detected", y = expression(paste("Number of points with ", lambda > 1)), tag = "C") +
    theme_light(base_size = 6)

a + b + c

ggsave(
    filename = "lambda_plots.png",
    plot = (a + b + c) ,
    device = "png",
    path = "paper/figures/",
    dpi = 300, width = 7.3, height = 3, bg = "white"
)
  