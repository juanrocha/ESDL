## Figures for ESDL early adopters report
## 

library(tidyverse)
library(earlywarnings)

# load("~/Documents/Projects/ESDL_earlyadopter/ESDL/190725_std_gpp.RData")
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/190728_slopes_std_chlorA_long-term-trend.RData')
load('~/Documents/Projects/ESDL_earlyadopter/190726_slopes_std_chlor_A_trend.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/190728_slopes_std_GPP_trend.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/190728_slopes_std_GPP_Long-Term-Variability.RData')

# dat <- as_tibble(dat)
# 
# dat %>%
#     ggplot(aes(window, std_gpp)) +
#     geom_boxplot(aes(group = window)) 


g2 <- df_slope %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
    #filter(lat < -5, lat > -44, lon > 100 , lon < 165 ) %>%
    ggplot(aes(lon, lat)) + 
    geom_raster(aes(fill = slope)) +
    # scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") + 
    labs(tag = "B", title = "Chlorophile A Trend", caption = "Data source: ESDL") + 
    #labs(tag = "D", title = "Gross Primary Productivity Trend", caption = "Data source: ESDL") + 
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    theme_void() +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))

# For Clorophile A trend
# load("190726_slopes_std_chlor_long-term-trend.RData")
# load("190726_slopes_std_chlor_A_trend.RData")
# 
# g2 <- df_slope %>%
#     separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
#     #filter(lat < -5, lat > -44, lon > 100 , lon < 165 ) %>%
#     ggplot(aes(lon, lat)) + 
#     geom_raster(aes(fill = slope)) +
#     scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
#     # scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
#     labs(tag = "B") + 
#     theme_void() +
#     theme(legend.position = "bottom")  + 
#     guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))


## Zoom ins

# df_slope %>%
#     separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
#     #filter(lat < 15, lon > -100 , lon < -25 ) %>%
#     ggplot(aes(lon, lat)) + 
#     geom_raster(aes(fill = slope)) +
#     # scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
#     scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
#     labs(tag = "A") + 
#     theme_minimal() +
#     theme(legend.position = "bottom")  + 
#     guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))


## Combine the figure

g1 <- g1+ theme_void(base_size = 6) +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))
g2 <- g2+ theme_void(base_size = 6) +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))
g3 <- g3+ theme_void(base_size = 6) +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))
g4 <- g4+ theme_void(base_size = 6) +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))
g <- list ( g1, g2, g3, g4)

source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:4), ncol = 2, nrow = 2, byrow = T)
quartz(width = 5, height = 5, pointsize = 6)

multiplot(plotlist = g, layout = layout)

quartz.save(file = 'Fig1.pdf', type = 'pdf', dpi = 300,
            pointsize = 5, family = "helvetica", width = 5, height = 5)