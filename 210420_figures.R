library(tidyverse)
library(tictoc)
library(rworldmap)
library(sf)
library(patchwork)
library(tidymodels)
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

# dataset with biomes:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

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

tt[["TRUE"]] / (tt[["TRUE"]] + tt[["FALSE"]])

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

#### Figure 1 ####

df_affected <- dat %>%
  filter(detected == TRUE) %>%
  group_by(biome) %>%
  summarise(affected_area = n())

df_affected <- df_affected %>%
  #left_join((dat %>% group_by(biome) %>% summarize(n = n())))
  #when marine:
  left_join((df_marine %>% group_by(biome) %>% summarize(n = n())))

g2 <- df_affected %>%
  mutate(proportion = affected_area / n) %>%
  ggplot(aes(fct_rev(biome), proportion)) +
  geom_col(aes(fill = biome), alpha = 0.75, show.legend = FALSE) +
  labs(tag = "B", y = "Proportion of area", x = "Biomes") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_light(base_size = 5) + theme(axis.text.y = element_blank())

g3 <- dat %>%
  filter(n_ews > 0) %>%
  mutate(n_ews = as_factor(n_ews)) %>%
  ggplot(aes(fct_rev(biome))) +
  geom_bar(aes(fill = biome, alpha = n_ews)) +
  guides(fill = "none", alpha = guide_legend(
    title.position = "top", keywidth = 0.25, keyheight = 0.3, label.position = "bottom")) +
  coord_flip() +
  scale_alpha_discrete("Signals",range = c(0.5,1)) +
  #scale_y_log10() +
  labs(tag = "C", y = "Area in pixels", x = "Biomes") +
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
  scale_alpha_discrete(name = "Detected") +
  scale_fill_hue(name = "Terrestrial biomes") +
  guides(
    fill = guide_legend(ncol = 4, title.position = "top"),
    alpha = guide_legend(ncol = 1, title.position = "top")
  ) +
  theme_void(base_size = 6) + labs(tag = "A") +
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
    xmin = -189, ymin = -60, xmax = -110, ymax = -20)
g1


ggsave(
  plot = g1,
  filename = "fig_detection_GPP.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 7, height = 4, dpi = 320
)


#### Marine figures: slightly different arrangement ####
g0 <- dat %>%
  ggplot(aes(lon,lat)) +
  geom_tile(aes(fill = biome, alpha = detected)) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  scale_alpha_discrete(name = "Detected") +
  scale_fill_hue(name = "Marine rehalms") +
  guides(
    fill = "none",
    alpha = guide_legend(ncol = 2, title.position = "left")
  ) +
  theme_void(base_size = 6) +#labs(tag = "C") + 
  ylim(c(-60,NA)) +
  theme(
    legend.position = c(0.75,0.1),
    legend.direction = "horizontal",
    legend.box = "horizontal", legend.title.align = 1,
    legend.margin = margin(t = 0, r = 5, b = 2, l = 5, unit = "pt"),
    legend.key.size = unit(0.15, "cm"),
    legend.text = element_text(size = 5),
    plot.background = element_rect(fill = "white")
  ) +
  annotation_custom(
    grob = ggplotGrob(g2),
    xmin = -40, ymin = 52, xmax = 80, ymax = 90) +
  annotation_custom(
    grob = ggplotGrob(g3),
    xmin = 80, ymin = 52, xmax = 175, ymax = 90)

g2 <- df_affected %>%
  mutate(proportion = affected_area / n) %>%
  ggplot(aes(fct_rev(biome), proportion)) +
  geom_col(aes(fill = biome), alpha = 0.75, show.legend = FALSE) +
  labs(y = "Proportion of area", x = "Marine rehalms") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_light(base_size = 6) #+ theme(axis.text.y = element_blank())

g3 <- dat %>%
  filter(n_ews > 0) %>%
  mutate(n_ews = as_factor(n_ews)) %>%
  ggplot(aes(fct_rev(biome))) +
  geom_bar(aes(fill = biome, alpha = n_ews)) +
  guides(fill = "none", alpha = guide_legend(
    title.position = "top", keywidth = 0.25, keyheight = 0.3, label.position = "right")) +
  coord_flip() +
  scale_alpha_discrete("Signals",range = c(0.5,1)) +
  #scale_y_log10() +
  labs(y = "Area in pixels", x = "Marine rehalms") +
  theme_light(base_size = 6) +
  theme(
    legend.position = c(0.8, 0.3),
    #legend.direction = "vertical",
    #legend.box = "vertical", legend.title.align = 0.5,
    #legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 5)) 



quartz(width = 7, height = 4)

g2 + g3 + plot_layout(widths = c(1,2))

ggsave(
  plot = last_plot(),
  filename = "fig_detection_marine.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 7, height = 5, dpi = 320
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
  scale_fill_datetime(name = "Year", low = "dodgerblue", high = "goldenrod") + 
  theme_light(base_size = 8)

ggsave(
  plot = last_plot(),
  filename = "sm_temporal_map.png",
  path = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/paper/figures/",
  device = "png",
  width = 6, height = 5, dpi = 320
)


g3 <- px_all %>% 
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
  scale_fill_viridis_c(option = "D") + labs(tag = "B") +
  theme_light(base_size = 6) #+ theme(legend.position  = "bottom")

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

load("Results/210529_rf_GPP.RData")

# load data:
load("Results/regression_data_GPP.RData")
#load("Results/regression_data_TER.RData")
# load("Results/regression_data_chlorA.RData")

deltas <- deltas %>% mutate(detected = n_ews != 0) %>%
  mutate(across(.cols = starts_with("delta"), abs, .names = "abs_{.col}")) %>%
  mutate(detected = as_factor(detected),
         detected = fct_relevel(detected, "TRUE", "FALSE")) %>%
  mutate(biome = fct_explicit_na(biome))


## Data split
data_split <- initial_split(
  deltas %>% filter(n_ews == 0 | n_ews >1), #, biome == "Boreal Forests/Taiga"
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
  geom_tile()


#### Supplementary material ####


stat_ews <- c("Standard deviation", "Autocorrelation lag-1", "Skewness",
 "Kurtosis", "Fractal dimension")
vals <- names(dat)[names(dat) %>% str_detect("abruptness_.")][c(5,1,4,3,2)]
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
      ) # +
      # annotation_custom(
      #   grob = ggplotGrob(
      #     ggplot(
      #       data = dat,
      #       aes(.data[[vals]], fct_rev(biome))) +
      #       geom_violin(aes(fill = biome, color = biome),
      #                    alpha = 0.5, show.legend = FALSE, size = 0.05
      #                  ) + #outlier.size = 0.1
      #       labs(y = "Biomes", x = expression(Delta)) +
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
  width = 7.5, height = 8, dpi = 320
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
