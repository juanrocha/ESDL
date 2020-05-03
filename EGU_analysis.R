## Figures for EGU meeting
## 200429

library(tidyverse)
library(earlywarnings)
library(progress)
library(segmented)
library(tictoc)
# library(future)


load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200428_std_GPP_fast-oscillations.RData')

skimr::skim(dat)
object.size(dat) %>% format("Mb")

dat %>%
    group_by(id) %>%
    summarize(n())
## 9 datapoints (probably not enough)

dat %>%
    filter(id == "-71.125_1.125") %>%
    ggplot(aes(y = std_gpp, x =window)) +
    geom_line()


## Linear models
tic()
models <- dat %>% 
    split(.$id) %>%
    map(~lm(std_gpp ~ window, data = .))
toc()
# 13.5 secs

## calculate slopes
pb <- progress_bar$new(total = 100)
slope <-  map_dbl(models, function(x){coef(x)[2]})

df_slope <- tibble(
    id = names(slope),
    slope = as.numeric(slope)
)


## Picewise regression
# df1 <- dat %>%
#     filter(id =="143.625_-34.125") %>% #somewhere in Melburn
#     as_data_frame() 
# 
# ols <- lm(std_gpp ~ window, data = df1) 
# 
# fit <- segmented(ols, seg.Z = ~ window, psi = 6)
# ## Break point and confidence intervals
# confint(fit)
# ## 
# slope(fit)
# ## test for change in slope:
# davies.test(fit, ~window, k = 5)
# 
# dat
# 
# segmented2 <- function(x){
#     fit <- lm()
# }
# 
# 
# ###
# tic()
# piecewise <- head(models) %>%
#     map(~{segmented.lm(., seg.Z = ~ window, psi = 6, data = .$model)})
# toc()

seggy <- safely(function(x){
    df <- x
    fit <- lm(std_gpp ~ window, data = df)
    sfit <- segmented::segmented(fit, ~ window, psi = 70)
    return(sfit)
})

## trying again 
tic()
models <- dat %>%
    head() %>%
    map(seggy_safe)
toc()


## Plots
g2 <- df_slope %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
    #filter(lat < -5, lat > -44, lon > 100 , lon < 165 ) %>%
    ggplot(aes(lon, lat)) + 
    geom_tile(aes(fill = slope)) +
    # scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") + 
    labs(tag = "B", title = "Fast Oscillations", caption = "Data source: ESDL") + 
    #labs(tag = "D", title = "Gross Primary Productivity Trend", caption = "Data source: ESDL") + 
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    #theme_void() +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))

## somewhere in Melbourne "143.625_-34.125"; Amazon 
dat %>%
    filter(id =="143.625_-34.125") %>%
    ggplot(aes(y = std_gpp, x =window)) +
    geom_line()

## calculate means and differences:
dat %>% 
    mutate(when = ifelse(window < 4, "start", 
                         ifelse(window > 6, "middle", "end"))) %>%
    group_by(id, when) %>%
    summarize(mean_std = mean(std_gpp)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = id, names_from = when, values_from = mean_std) %>%
    mutate(diff = end - start) %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
    #filter(lat < -5, lat > -44, lon > 100 , lon < 165 ) %>%
    ggplot(aes(lon, lat)) + 
    geom_tile(aes(fill = diff)) +
    # scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") + 
    labs(tag = "B", title = "Fast Oscillations", caption = "Data source: ESDL") + 
    #labs(tag = "D", title = "Gross Primary Productivity Trend", caption = "Data source: ESDL") + 
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    #facet_wrap(~when) +
    theme_void() +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))


### graph with one map per window.
dat %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
    #filter(lat < -5, lat > -44, lon > 100 , lon < 165 ) %>%
    ggplot(aes(lon, lat)) + 
    geom_tile(aes(fill = std_gpp)) +
    # scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") + 
    labs(tag = "B", title = "Fast Oscillations", caption = "Data source: ESDL") + 
    #labs(tag = "D", title = "Gross Primary Productivity Trend", caption = "Data source: ESDL") + 
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    facet_wrap(~window) +
    theme_void() +
    theme(legend.position = "bottom")  + 
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))