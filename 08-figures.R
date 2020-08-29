library(tidyverse)
library(tictoc)

# load results:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200805_results_ews_halfwindow_gpp.RData')


## join the ecosystem information:
df <- left_join(df, df_biomes)

# map the biomes
df %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = biome)) +
    theme_void()

## Comparison of maps
df %>% filter(stat == "std") %>%
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c() +
    facet_grid(stat ~ feature)
## comparison of distributions:
df %>% filter(stat == "fd") %>%
    filter(feature == "diff") %>%
    #left_join(df_biomes) %>%
    ggplot(aes(value, group = biome)) +
    geom_density(aes(fill = biome), alpha = 0.5) +
    geom_vline(
        xintercept = quants, color = "grey60", size = 0.5) +
    annotate(
        "text", x = quants, y = 6,
        label = c("50%","75%", "90%", "95%"),
        hjust = 0)+
    geom_rug(aes(color = value)) +
    # scale_color_gradient2(
    #   high = 'red', low = 'black',  midpoint = quants[1],
    #   mid = 'yellow', na.value = "gray84") +
    scale_color_viridis_c(option = "C") +
    #scale_x_log10() +
    # facet_wrap(~feature, scales = "free") +
    theme_light()


## use only the upper qunatile on the difference:
quants <- df %>% filter(stat == "fd") %>%
    filter(feature == "diff") %>%
    pull(value) %>%
    quantile(prob = c(0.5, 0.75,0.9, 0.95))

df %>% filter(stat == "fd") %>%
    filter(feature == "diff") %>%
     #%>%
    ggplot(aes(value, biome)) +
    geom_boxplot(aes(fill = biome), alpha = 0.5, show.legend = FALSE) +
    # geom_vline(
    #     xintercept = quants, color = "grey60", size = 0.5, linetype = 2) +
    # annotate(
    #     "text", x = quants, y = 17.5,
    #     label = c("50%","75%", "90%", "95%"),
    #     hjust = 0)+
    #geom_rug(aes(color = value), size = 0.25) +
    #scale_color_viridis_c() +
    facet_wrap(~stat, scales = "free_x") +
    theme_light()

quants_all <- df %>%
    filter(feature == "diff") %>%
    group_by(biome, stat) %>%
    summarize(q75 = quantile(value, prob = 0.9))



## large differences in std are >0.3 : the tail of the distribution
df %>% filter(stat == "std") %>%
  filter(feature == "diff") %>%
  ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c(option = "C") +
  # scale_fill_gradient2(
  #   high = 'orange', low = 'black',  midpoint = quants[1],
  #   mid = 'gray50', na.value = "gray84") +
  theme_void()



df %>% filter(stat == "std") %>%
    filter(feature == "diff") %>%
    filter(value > quant90)
## checking values in variance instead of std.
# df %>%
#     filter(stat == "std") %>%
#     mutate(var = value ^ 2) %>%
#     ggplot(aes(var)) +
#     geom_density() +
#     geom_rug(aes(color = var)) +
#     scale_color_viridis_c() +
#     facet_wrap(~feature, scales = "free") +
#     theme_light()
