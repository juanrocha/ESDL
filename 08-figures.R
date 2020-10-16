library(tidyverse)
library(tictoc)
library(rworldmap)
# library(cleangeo)
data("coastsCoarse")

# load results:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200918_summary_chlorA.RData')

# dataset with biomes:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

## join the ecosystem information:
df <- left_join(df, df_biomes)
df <- left_join(df, df_marine) # when marine systems
## Notice that I get >100k NAs mainly on the coasts that fasterize did not catch up.
## Make a note in the paper and add a map of missing values in SM.
df$biome_code[is.na(df$biome_code)] <- 100
# df$biome[is.na(df$biome)] <- "Unclassified"

# df <- df %>%
#   mutate(biome = fct_explicit_na(biome, na_level = "Unclassified"))

# map the biomes
# df %>%
#     ggplot(aes(lon, lat)) +
#     geom_tile(aes(fill = biome), show.legend = TRUE) +
#     theme_void()

## Comparison of maps: not useful for paper, I cannot chose color bar per plot
# df %>% filter(stat == "std") %>%
#     ggplot(aes(lon,lat)) +
#     geom_tile(aes(fill = value)) +
#     scale_fill_viridis_c() +
#     facet_grid(stat ~ feature)

## use only the upper qunatile on the difference:
quants <- df %>% filter(stat == "std") %>%
  filter(feature == "diff") %>%
  pull(value) %>%
  quantile(prob = c(0.5, 0.75,0.9, 0.95))

## comparison of distributions:
# g1 <- df %>% filter(stat == "std") %>%
#     filter(feature == "diff") %>%
#     #left_join(df_biomes) %>%
#     ggplot(aes(value, group = biome)) +
#     geom_density(aes(fill = biome), alpha = 0.25) +
#     geom_vline(
#         xintercept = quants, color = "grey60", size = 0.5) +
#     annotate(
#         "text", x = quants, y = 9,
#         label = c(" 50%"," 75%", " 90%", " 95%"),
#         hjust = 0)+
#     geom_rug(aes(color = value), show.legend = FALSE) +
#     # scale_color_gradient2(
#     #   high = 'red', low = 'black',  midpoint = quants[1],
#     #   mid = 'yellow', na.value = "gray84") +
#     scale_color_viridis_c(option = "C") +
#     #scale_x_log10() +
#     # facet_wrap(~feature, scales = "free") +
#   labs(tag = "A", xlab = "Absolute difference in standard deviation") +
#   theme_light(base_size = 8) +
#   theme(legend.position = c(0.8, 0.55))


g1 <- df %>% filter(stat == "std") %>%
  filter(feature == "diff") %>%
  ggplot(aes(value, fct_rev(biome))) +
  geom_boxplot(aes(fill = biome), alpha = 0.5, show.legend = FALSE,
    size = 0.1, outlier.size = 0.1) +
  labs(tag = "A", x = "Absolute difference in standard deviation", y = "Biomes") +
  geom_vline(
      xintercept = quants, size = 0.25, linetype = 2, color = "red") +
  annotate(
      "text", x = quants, y = 13.8, #y = 18.2, # terrestrial
      label = c(" 50%"," 75%", " 90%", " 95%"),
      hjust = 0, size = 1.5)+
  geom_rug(aes(color = value), size = 0.15, show.legend = FALSE,
    sides = "b", outside = FALSE) +
  scale_color_viridis_c(option = "C") +
  scale_y_discrete(expand = expansion(mult = c(0.1,0.1))) +
  coord_cartesian(clip = "off") +
  theme_light(base_size = 6)
  # theme(axis.text.y = element_blank())

quants_all <- df %>%
    filter(feature == "diff") %>%
    group_by(biome, stat) %>%
    summarize(q90 = quantile(value, prob = 0.9))

## pull out the pixels where the difference in the statistic is in the 90% quantile

df <- left_join(df, quants_all) %>%
  mutate(q90_plus = value > q90)


## large differences in std are >0.3 : the tail of the distribution
# df %>% filter(stat == "std") %>%
#   filter(feature == "diff") %>%
#   ggplot(aes(lon, lat)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(option = "C") +
#   theme_minimal()

stat_ews <- c("std", "ac1", "skw", "kur", "fd")
plot_maps <- list()
tags <- LETTERS[1:5]

tic()
plot_maps <- pmap(
  list(stat_ews, tags),
  function(stat_ews, tags){
    df %>%
      filter(stat == stat_ews, feature == "diff") %>%
      ggplot(aes(lon, lat)) +
      geom_tile(aes(fill = value, alpha = q90_plus)) + # , alpha = q75_plus
      scale_fill_viridis_c(option = "C", name = expression(Delta)) +
      scale_alpha_discrete(range = c(0.3,1), name = "Percentile", label = c("< 90%", "> 90%")) +
      guides(
        fill = guide_colourbar(
          title.position = "top",barwidth = 3, barheight = 0.3, order = 2),
        alpha = guide_legend(
          title.position = "top", keywidth = 0.3, keyheight = 0.3, order = 1)) +
      theme_void(base_size = 6) + labs(tag = tags) +
      theme(
        legend.position = "bottom", #c(0.7,0.1), 
        legend.direction = "horizontal",
        legend.box = "horizontal", legend.title.align = 0.5,
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
      ) #+
      # annotation_custom(
      #   grob = ggplotGrob(
      #     ggplot(
      #       data = (df %>% filter(stat == stat_ews, feature == "diff")),
      #       aes(value, fct_rev(biome))) +
      #       geom_boxplot(aes(fill = biome, color = biome),
      #                    alpha = 0.5, show.legend = FALSE, size = 0.1,
      #                    outlier.size = 0.1) +
      #       labs(y = "Biomes", x = expression(Delta)) +
      #       theme_minimal(base_size = 6) + theme(axis.text.y = element_blank())
      #   ), xmin = -185, ymin = -60, xmax = -90, ymax = 10)
  }
)
toc() # 40 secs

### summary bar plots:
df_ews <- df %>%
  filter(q90_plus == TRUE, feature == "diff") %>%
  group_by(lon, lat, biome) %>% # ggplot(aes(biome)) + geom_bar() + coord_flip()
  summarise(n_ews = n())

g3 <- df_ews %>%
  mutate(n_ews = as_factor(n_ews)) %>%
  ggplot(aes(fct_rev(biome))) +
  geom_bar(aes(fill = biome, alpha = n_ews)) +
  guides(fill = "none", alpha = guide_legend(
    title.position = "top", keywidth = 0.3, keyheight = 0.3, label.position = "bottom")) +
  coord_flip() +
  scale_alpha_discrete("Number of\nsignals",range = c(0.5,1)) +
  #scale_y_log10() +
  labs(tag = "C", y = "Area in pixels", x = "Biomes") +
  theme_light(base_size = 6) +
  theme(legend.position = c(0.85,0.15), legend.direction = "horizontal",
        legend.box = "horizontal", legend.title.align = 0.5,
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.text.y = element_blank())

plot_maps[[6]] <-  df_ews %>%
  ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = n_ews), show.legend = TRUE) +
  geom_path(aes(long,lat, group = group), data = coastsCoarse, size=0.1) +
  #scale_alpha(range = c(0.5,1)) +
  scale_fill_viridis_c("Number of signals", option = "C") +
  labs(tag = "F") + ylim(-54.625, 79.875) + #xlim(-179.875,179.875) +
  guides(fill = guide_colourbar(
      title.position = "top",barwidth = 3, barheight = 0.2)) +
  theme_void(base_size = 6) +
  theme(
    legend.position = "bottom", #c(0.7,0.1), 
    legend.direction = "horizontal",
    legend.box = "horizontal", legend.title.align = 0.5,
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) #+
  # annotation_custom(
  #   grob = ggplotGrob(
  #     ggplot(
  #       data = (df_ews %>% mutate(n_ews = as_factor(n_ews))),
  #       aes(fct_rev(biome))) +
  #       geom_bar(aes(fill = biome, alpha = n_ews), show.legend = FALSE) +
  #       scale_alpha_discrete(range = c(0.5,1)) +
  #       scale_y_log10() +
  #       labs(x = "Biomes", y = "Pixels") +
  #       coord_flip() +
  #       theme_minimal(base_size = 6) +
  #       theme(axis.text.y = element_blank())
  #   ), xmin = -185, ymin = -60, xmax = -90, ymax = 10)

## calculate the bar plots also as proportion of the biome showing EWS.

df_affected <- df_ews %>%
  group_by(biome) %>%
  summarise(affected_area = n())

df_affected <- df_affected %>%
  left_join((df_biomes %>% group_by(biome) %>% summarize(n = n())))
  #when marine:
  #left_join((df_marine %>% group_by(biome) %>% summarize(n = n())))

## correct the NA's induced in the unclassified biomes:
df_affected$n[is.na(df_affected$n)] <- df %>% filter(stat == "std", feature == "diff", biome_code == 100) %>% nrow()

g2 <- df_affected %>%
  mutate(proportion = affected_area / n) %>%
  ggplot(aes(fct_rev(biome), proportion)) +
  geom_col(aes(fill = biome), alpha = 0.75, show.legend = FALSE) +
  labs(tag = "B", y = "Proportion of area", x = "Biomes") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_light(base_size = 6) + theme(axis.text.y = element_blank())


### ensamble plot:
library(patchwork) # not working well with map alignment
library(gridBase)
library(grid)
library(gridExtra)

##J200904: Check ?gridExtra::arrangeGrob vignette for full control on sizing of layout. The patchwork option is not working well.

## Size aimed max: 183mm * 247mm ~ 7.2in * 9.72in but plus space for legend.
quartz(width = 7.2, height = 9)

# design <- c(
#   area(1,1,2,3),
#   area(3,4),
#   area(5,6),
#   area(7,8)
# )
top <- g1 + g2 + g3 + plot_layout(
  widths = unit(c(60, 45, 45), 'mm'),
  heights = unit(45, "mm"))

bottom <- wrap_plots(plot_maps) +
  plot_layout(nrow = 3, ncol = 2,
    widths = unit(c(90, 90), "mm"),
    heights = unit(c(5,5,5), "cm"))

# ## Combine the figure
g <- c(list ( g1, g2, g3) , plot_maps )

source('~/Dropbox/Code/multiplot.R')
layout <- matrix(
  c(1,1,2,3,
    4,4,5,5,
    4,4,5,5,
    6,6,7,7,
    6,6,7,7,
    8,8,9,9,
    8,8,9,9),
  ncol = 4, nrow = 7, byrow = TRUE)

multiplot(plotlist = g, layout = layout)

# quartz.save(file = 'Schill_NCC_Fig1.png', type = 'png', dpi = 300,
#             pointsize = 5, family = "helvetica", width = 5, height = 2)

quartz.save(file = "figS1_GPP.pdf", type = "pdf", width = 7, height = 7, dpi = 600)
## world map as template: contain countries
world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +
  #coord_map(projection = "mercator" ) #
  coord_quickmap() + theme_void()




continents <- coastsCoarse %>%
  st_as_sf()

ggplot() + geom_sf(data = continents) + coord_sf()

coastsCoarse %>% ggplot() + geom_path(aes(long,lat, group = group))
### Advise from https://stackoverflow.com/questions/20146809/how-can-i-plot-a-continents-map-with-r/20150341#20150341

# library(rworldmap)
# library(rgeos)
# library(maptools)
# library(cleangeo)  ## For clgeo_Clean()
#
# sPDF <- getMap()
# sPDF <- clgeo_Clean(sPDF)  ## Needed to fix up some non-closed polygons
# cont <-
#     sapply(levels(sPDF$continent),
#            FUN = function(i) {
#                ## Merge polygons within a continent
#                poly <- gUnionCascaded(subset(sPDF, continent==i))
#                ## Give each polygon a unique ID
#                poly <- spChFIDs(poly, i)
#                ## Make SPDF from SpatialPolygons object
#                SpatialPolygonsDataFrame(poly,
#                                         data.frame(continent=i, row.names=i))
#            },
#            USE.NAMES=TRUE)
#
# ## Bind the 6 continent-level SPDFs into a single SPDF
# cont <- Reduce(spRbind, cont)
#
# ggplot() + geom_path(data = cont, aes(long,lat, group = group), color = "grey90")


#### Figures for presentation:

quartz(width = 7.5, height = 2)

top <- g1 + g2 + g3 + plot_layout(
  widths = unit(c(40, 40, 40), 'mm'),
  heights = unit(40, "mm"))

top

quartz.save(file = "figures/figS1_chlorA.png", type = "png", width = 7.5, height = 2, dpi = 800)

quartz(width = 7.5, height = 7.5)

bottom <- wrap_plots(plot_maps) +
  plot_layout(nrow = 3, ncol = 2,
              widths = unit(c(90, 90), "mm"),
              heights = unit(c(5,5,5), "cm"))

bottom


quartz.save(file = "figures/figS2_maps_chlorA.png", type = "png", 
            width = 7.5, height = 7.5, dpi = 800)
