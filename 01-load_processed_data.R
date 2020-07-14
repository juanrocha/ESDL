## Load processed data:
## J200704

library(tidyverse)
library(tictoc)
library(future)
library(furrr)
library(tsibble)


setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_gpp")
files <- list.files()

#load(files[2])
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_gpp.RData')


read_all <- function(x){
    load(x)             # load the file, all objects are called mat
    ## make into a data frame
    df <- as_tibble(mat) %>%
        mutate(time = time) %>%
        pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
        filter(!is.na(gpp))
    return(df)
}

# do it in parallel
plan(multicore, workers = 10)
tic()
results <- files %>%
    future_map(read_all, .progress = TRUE)
toc() # 1min

object.size(results) %>% format("Gb") # 3.9Gb in RAM, cannot do that on high res

# latitudes are wrong, recover them from file names
lat <- files %>% str_remove("lat_") %>% str_remove(".RData") %>% as.numeric()
## Add corrected latitudes
tic()
results <- results %>%
    map2(., lat, function(x,y) x %>% add_column(lat = y))
toc()

results <- results %>% bind_rows()

## Calculate early warnings:
tic()
results <- results %>%
    mutate(lon = as.numeric(lon)) %>%
    group_by(lat, lon) %>%
    mutate(
        std_1year = slide_dbl(gpp, sd, na.rm = TRUE, .size = 52),
        std_4year = slide_dbl(gpp, sd, na.rm = TRUE, .size = 52*4),
        std_half = slide_dbl(gpp, sd, na.rm = TRUE, .size = length(time)/2)
    ) %>%
    mutate(key = row_number())
toc() # 1.4hrs

tic()
df <- results %>% filter(!is.na(std_half))
toc() # 14 sec

tic()
df <- df %>%
    select(-std_4year, -std_half) %>%
    mutate(
        first_diff = slide_dbl(std_1year, diff, .size = 2),
        increased = first_diff > 0)
toc() # 8 mins

tic()
ind <- df %>%
    summarize(
        increase = sum(increased, na.rm = TRUE),
        decrease = sum(!increased, na.rm = TRUE),
        driff = sum(first_diff, na.rm = TRUE))
toc() # 2.7 secs


ind %>%
    mutate(ratio = increase/decrease) %>%
    ggplot(aes(lon,lat)) + geom_tile(aes(fill = driff)) +
    scale_fill_gradient2(high = 'orange', low = 'blue', mid = 'grey60', na.value = "grey84", midpoint = 0)

    +
    geom_polygon(
        data = map_data("world") %>%
            rename(lon = long),
        aes(lon, lat), color = "white", size = 0.5)


world + g1
# I'm creating a list with the results per SD window to see if they hold
# comparison <- list()
# comparison$std_half <- ind
# comparison$std_4year <- ind
comparison$std_1year <- ind

ggplot(ind, aes(decrease)) + geom_density()






test <- df %>%
    filter(lat == 9.875, lon == 126.125) %>%
    select(-std_1year, -std_4year)

test <- test %>% mutate(
    first_diff = slide_dbl(std_half, diff, .size = 2)
)

ggplot(test, aes(gpp)) +
    geom_density() +
    geom_rug(aes(color = time)) +
    geom_vline(aes(xintercept = quantile(test$gpp, .5)))

ggplot(test, aes(gpp, std_half)) +
    geom_density2d(color = "black") +
    geom_line(aes(color = time), size = 0.5)
