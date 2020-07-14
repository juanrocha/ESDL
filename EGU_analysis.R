## Figures for EGU meeting
## 200429

library(tidyverse)
library(earlywarnings)
library(progress)
library(segmented)
library(tictoc)

### About parallel computing
### There is many ways of working in parallel, but two available options are:
### a) Working with `future`, `purrr` and `furrr` which are `purrr` friendly
### b) working with `parallel` which is the normal option with base R. It has a
### 'mclapply()' function that allow the setting of how many mc.cores = X to use
### However, it does not do well catching up errors, if one of the elements of
### the sublist fails, all the core returns empty results. I prefer `purrr` but
### my understanding is that these are packages under development.
library(future)
library(furrr)

## mapping libraries
library(rnaturalearth)

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200504_std_GPP_fast-oscillations.RData')

skimr::skim(dat)
object.size(dat) %>% format("Mb")

dat %>%
    group_by(id) %>%
    summarize(n())
## 9 datapoints (probably not enough)

dat %>%
    filter(id == "-89.125_79.875") %>%
    ggplot(aes(y = std_gpp, x =window)) +
    geom_line()


## Linear models
# tic()
# models <- dat %>%
#     split(.$id) %>%
#     map(~lm(std_gpp ~ window, data = .))
# toc()
# 13.5 secs
object.size(models) %>% format("Gb")
# ## calculate slopes
# pb <- progress_bar$new(total = 100)
# slope <-  map_dbl(models, function(x){coef(x)[2]})
#
# df_slope <- tibble(
#     id = names(slope),
#     slope = as.numeric(slope)
# )


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

seggy <- function(x){
    df <- x
    fit <- lm(std_gpp ~ window, data = df)
    sfit <- segmented::segmented(fit, ~ window, psi = 6)
    davies <- segmented::davies.test(fit , ~window, k = 5)
    return(list(sfit, davies))
}

seggy_safe <- safely(seggy)

### Make dat into a list by pixel
# tic()
# dat <- dat %>%
#   split(.$id)
# toc()

### Notice I'm wasting memory by storing the name of each data frame with coordinates AND the id in each of them. Create a vector with the coordinates and delete the id. By the order they can be matched again.
id <- names(dat)
tic()
dat <- dat %>%
    map(function(x){x %>% select(-id)})
toc() # > 5min, dat is now ~250Mb



##################################
## trying working on parallel
plan(multicore, workers = 10) # only use in Mac outside RStudio
## without parallel it takes 0.271 sec for 5 elements =
(((0.271 / 5) * length(dat))/60)/60
## 3.17 hours in sequence. With 10 cores/ workers, hopefully that goes down to 30+mins
tic()
models <- dat %>%
    # head() %>%
    future_map(seggy_safe, .progress = TRUE)
toc() ## 78 minutes
## models is 5.7Gb in memory... treat it with care.
object.size(models) %>% format("Gb")

### extract the slopes:
plan(multicore, workers = 10)
slopes <- list()
tic()
for (i in 1:length(dat)){
    ifelse(
        class(models[[i]][[1]][[1]])[1] == "segmented", # conditional clause
        slopes[[i]]<- models[[i]]$result[[1]] %>%
            segmented::slope() %>%
            unlist() %>%
            enframe() %>%
            filter(name == "window1" | name == "window2") %>%
            pivot_wider(names_from = name, values_from = value) %>%
            rename(slope1 = window1, slope2 = window2) %>%
            mutate(diff = slope2 - slope1),                         # TRUE
        slopes[[i]] <- tibble(slope1 = NA, slope2 = NA, diff = 0)     # FALSE
    )
}
toc() # 45min
object.size(slopes) %>% format("Gb")

slopes %>%
    bind_rows() %>%
    ggplot(aes(diff)) +
    geom_density()

slopes %>%
    bind_rows() %>%
    mutate(id = names(models)) %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = diff)) +
    scale_fill_gradient2(high = 'black', low = 'dodgerblue', mid = 'grey84', na.value = "grey50") +
    theme_void()

# getwd()
#save(slopes, file = "200504_slopes_sd_GPP.RData")

# p-values
tic()
p_values <- 1:length(models) %>%
    future_map(
        function(x){
            tibble(
                p_value = models[[x]][[1]][[2]]$p.value,
                break_point = models[[x]][[1]][[2]]$statistic
            )
        }
    )
toc() # 187.63 sec elapsed ~ 3mins!!
# save(p_values, file = "200504_p_values_sd_GPP.RData")

errors <- 1:length(models) %>%
    future_map(~!is.null(models[[.]]$error))

errors %>% unlist() %>% sum() ## zero errors :)

##### Now you can close down and open R again to process figures:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200504_std_GPP_fast-oscillations.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200504_p_values_sd_GPP.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200504_slopes_sd_GPP.RData')

id <- names(dat)

slopes <- slopes %>%
    bind_rows() %>%
    mutate(id = id)
p_values <- p_values %>%
    bind_rows() %>%
    mutate(id = id)

df_dat <- left_join(slopes, p_values)
## fix coordinates
df_dat <- df_dat %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE)
## Object is 12.9Mb
tic()
df_dat %>%
    ggplot() +
    #geom_density(aes(slope1), fill = "blue", alpha = 0.5) +
    geom_density(aes(diff), fill = "red", alpha = 0.5) +
    theme_light()
toc()


## Plots
## world map as template
# world <- ggplot(
#     map_data("world") %>% rename(lon = long),
#     aes(x = lon, y = lat)) +
#     geom_polygon(aes(group = group), color = "grey65",
#                fill = "#f9f9f9", size = 0.2) +
#     #coord_map(projection = "mercator" ) #
#     coord_quickmap() + theme_void()

# world <- ne_countries(scale = "medium", returnclass = "sf")
# world_map <- ggplot(data = world) + geom_sf(color = "grey65",
#                 fill = "#f9f9f9", size = 0.2) +
#                 coord_sf() + theme_void()

g3 <- df_dat %>%
    # filter(lat < -5, lat > -44, lon > -163 , lon < -55 ) %>% # australia
    # filter(lat < 75, lat > 20, lon > -165 , lon < -50 ) %>% # north america
    # filter(lat < 15, lat > -60, lon > -80 , lon < -30 ) %>% # south america
    # filter(lat < 36, lat > -40, lon > -19 , lon < 57 ) %>% # africa
    # filter(lat < 80, lat > 9, lon > -16 , lon < 170 ) %>% # Eurasia
    # filter(p_value < 0.05, slope2> 0.05) %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = diff)) +
    scale_fill_gradient2(high = 'blue', low = 'red', mid = 'grey84', na.value = "grey50") +
    #scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") +
    labs(title = "Gross Primary Productivity",
        subtitle = "Difference in slopes, time rolling window (50 weeks) of standard deviations",
        caption = "Data source: ESDL, @juanrocha") +
    #labs(tag = "D", title = "Gross Primary Productivity", caption = "Data source: ESDL") +
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    theme_void() +
    theme(legend.position = "bottom"
        #plot.background = element_rect(fill = "gray50"),
        #panel.background = element_rect(fill = "gray50")
    )  +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))

### One pixel
df_dat %>% filter(p_value < 0.05, diff > 0.1, slope2 > 0.22)
"91.875_75.375" "-13.875_16.625" "112.125_-2.875"

case <- dat[[which(id == "-13.875_16.625")]] %>% seggy

g1 <- dat[[which(id == "-13.875_16.625")]] %>%
    ggplot(aes(x = window, y = std_gpp)) +
    geom_line(size = 0.5) +
    geom_vline(aes(xintercept = 64.5), color = "purple") +
    geom_abline(aes(intercept = 3.286e-01, slope = 7.634e-05), color = "red") +
    geom_abline(aes(intercept = 0.135 , slope = 3.0757e-03), color = "blue") +
    labs(title = "Rolling window (50 weeks) of a pixel", y = "Standard Deviation of Gross Primary Productivity") +
    theme_light()


## timing
g4 <- df_dat %>%
    #filter(p_value < 0.05, diff > 0.05) %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = break_point)) +
    #scale_fill_gradient2(high = 'dodgerblue', low = 'orange', mid = 'grey84', na.value = "grey50") +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") +
    labs(title = "Timing", subtitle = "Breaking points -- when the SD change slope -- also cluster in time",caption = "Data source: ESDL; @juanrocha") +
    #labs(tag = "D", title = "Gross Primary Productivity", caption = "Data source: ESDL") +
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    theme_void() +
    theme(legend.position = "bottom"
        #plot.background = element_rect(fill = "gray50"),
        #panel.background = element_rect(fill = "gray50")
    )  +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))

setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/figures")
ggsave(g4, filename = "timming.png", device = "png",
     width = 7, height = 5)





# ## somewhere in Melbourne "143.625_-34.125"; Amazon
# dat %>%
#     filter(id =="143.625_-34.125") %>%
#     ggplot(aes(y = std_gpp, x =window)) +
#     geom_line()

## calculate means and differences:
# dat %>%
#     mutate(when = ifelse(window < 4, "start",
#                          ifelse(window > 6, "middle", "end"))) %>%
#     group_by(id, when) %>%
#     summarize(mean_std = mean(std_gpp)) %>%
#     ungroup() %>%
#     pivot_wider(id_cols = id, names_from = when, values_from = mean_std) %>%
#     mutate(diff = end - start) %>%
#     separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
#     #filter(lat < -5, lat > -44, lon > 100 , lon < 165 ) %>%
#     ggplot(aes(lon, lat)) +
#     geom_tile(aes(fill = diff)) +
#     # scale_fill_gradient2(high = 'red', low = 'blue', mid = 'grey50', na.value = "grey50") +
#     scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
#     # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") +
#     labs(tag = "B", title = "Fast Oscillations", caption = "Data source: ESDL") +
#     #labs(tag = "D", title = "Gross Primary Productivity Trend", caption = "Data source: ESDL") +
#     # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
#     #facet_wrap(~when) +
#     theme_void() +
#     theme(legend.position = "bottom")  +
#     guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))
#

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
