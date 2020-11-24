## This scipts separates critical slowing down from critical speeding up
## and plots some preliminary results that can be used as figure 1.

library(tidyverse)
library(tictoc)
library(future)

# load results:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_summary_gpp_log.RData')
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_detected_gpp_log.RData")
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201023_segmented_gpp_log.RData')
## import time series only for selected pixels:
df_ews %>%
    filter(n_ews == 5) %>%
    group_by(biome) %>%
    tally()
## select the latitudes of two pixes with high detection
lats <- df_ews %>%
    filter(n_ews == 5) %>%
    filter(biome == "Tropical and Subtropical Moist Broadleaf Forests") %>%
    pull(lat)
lons <- df_ews %>%
    filter(n_ews == 5) %>%
    filter(biome == "Tropical and Subtropical Moist Broadleaf Forests") %>%
    pull(lon)
## read the raw data files for such lats
lats <- as.character(lats)
path <- "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/ews_halfwindow_gpp_log/"
files <- paste(path, "lat_",lats, ".csv", sep = "")

tic()
dat <- files %>%
    map(function(x){
        read_csv(
            file = x,
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
    })
toc() # 13 secs

## Remember: there is an error on the fractal dimension, the first value is always zero. Remove it. The problem is that the position of the points changes from one dataset to another.
dat <- dat %>%
    map(function(x){
        x$ews_fd[x$ews_fd == 0] <- NA
        return(x)
    })

# 2 pixels, each one by lon
df_pixel <- map2(dat, lons,
     function(x,y){
         x %>% filter(lon %in% y)
     })

df_pixel <- df_pixel %>%
     map2(.x = ., .y = lats, function(x,y) {
         x$lat <- y
         return(x)
     }) %>%
     bind_rows() %>%
     mutate(lat = as.numeric(lat)) # now you know where they are.

i = 8
annotation_df <- df_pixel %>%
    filter(lon == lons[i], lat == lats[i]) %>%
    dplyr::select(-lon, -gpp, -lat) %>%
    pivot_longer(cols = 2:last_col(), names_to = "stat", values_to = "value") %>% mutate(stat = as_factor(stat)) %>% group_by(stat) %>%
    summarize(
        y_min = min(value, na.rm = TRUE),
        y_max = max(value, na.rm = TRUE),
        y_mid = (y_max - y_min ),
        x_min = min(time), x_max = lubridate::ymd_hms("2009-12-31 00:00:00"),
        y = y_max - (y_mid/2))

annotation_df$x_min[str_which(annotation_df$stat, "ews_")] <- annotation_df$x_max[1]

annotation_df$y_min[annotation_df$stat == "gpp_1d"] <- -3
annotation_df$y_max[annotation_df$stat == "gpp_1d"] <- -3
annotation_df$y[annotation_df$stat == "gpp_1d"] <- -4

annotation_df$label <- c(
    "rolling~window", "Delta~standard~deviation","Delta~autocorelation~lag-1",
    "Delta~kurtosis","Delta~skewness","Delta~fractal~dimension")

annotation_df$x <- c(rep(lubridate::ymd_hms("2006-01-05 00:00:00"), 1),
    rep(lubridate::ymd_hms("2009-07-31 00:00:00"), 5) )

annotation_df <- annotation_df %>%
    mutate(stat = fct_recode(
    stat, "Processed time series" = "gpp_1d",
    "Standard deviation" = "ews_std",
    "Autocorrelation lag-1" = "ews_ac1",
    "Kurtosis" = "ews_kur", "Skewness" = "ews_skw",
    "Fractal dimension" = "ews_fd"))


df_pixel %>%
    filter(lon == lons[i], lat == lats[i]) %>%
    dplyr::select(-lon, -gpp, -lat) %>%
    # filter(ews_fd != 0) %>%
    pivot_longer(cols = 2:last_col(), names_to = "stat", values_to = "value") %>%
    mutate(stat = as_factor(stat)) %>%
    mutate(stat = fct_recode(
        stat, "Processed time series" = "gpp_1d",
        "Standard deviation" = "ews_std",
        "Autocorrelation lag-1" = "ews_ac1",
        "Kurtosis" = "ews_kur", "Skewness" = "ews_skw",
        "Fractal dimension" = "ews_fd")) %>%
    ggplot(aes(time,value)) +
    geom_line(size = 0.1) +
    geom_segment(
        data = annotation_df,
        aes(x = x_min, y = y_min, xend = x_max, yend = y_max),
        color = "dodgerblue", size = 0.5) +
    geom_text(data = annotation_df, aes(x,y, label = label),
        parse = TRUE, size = 2, color = "dodgerblue",
        hjust = c(0.5, rep(1,5))) +
    # # This chuck add the break point result from the segmented regression.
    geom_point(
        data = px_results %>%
            filter(lon == lons[i], lat == lats[i]) %>%
            mutate(stat = as_factor(stat),
                stat = fct_recode(stat,
                "Standard deviation" = "std",
                "Autocorrelation lag-1" = "ac1",
                "Kurtosis" = "kur", "Skewness" = "skw",
                "Fractal dimension" = "fd")) %>%
            left_join(select(annotation_df, stat, y)) %>%
            rename(value = y),
        aes(time, value)
    ) +
    facet_wrap(vars(stat), scales = "free_y", nrow=6) +
    theme_light(base_size = 5)

# quartz.save(file = "figures/figS5_one_pixel_GPP.png", type = "png",
#             width = 3, height = 5, dpi = 600)



######## Now identify the break point with the segmented approach:
## code snippets from 03-EWS_segmented_regression.R

df_pixel <- df_pixel %>%
    filter(!is.na(ews_fd)) %>%
    group_by(lon, lat) %>%
    mutate(week = row_number()) %>%
    dplyr::select(-gpp, -gpp_1d) %>%
    pivot_longer(cols = starts_with("ews"), names_to = "stat", values_to = "value")

seggy <- function(x){
    dfr <- x %>%
        filter(!is.na(value)) # get rid of missing values
    fit <- lm(value ~ week, data = dfr)
    # no predefined psi (break point)
    sfit <- segmented::segmented(fit, ~ week)
    # no predefined k (number of points to evaluate hypothesis)
    davies <- segmented::davies.test(fit , ~ week, k = 10)
    return(list(sfit, davies))
}

seggy_safe <- safely(seggy)

# prepare keys for later recover
tic()
keys <- df_pixel %>%
    group_by(lon,lat, stat) %>%
    group_keys()
# split by lon
df_pixel <- df_pixel %>%
    group_by(lon,lat, stat) %>%
    group_split()
toc() # 0.32 secs


## Apply seggy to all pixels
tic()
models <- df_pixel %>% map(seggy_safe)
toc() # 2.3 sec in sequential

## extract slopes:
slopes <- list()
tic()
for (i in 1:length(models)){
    ifelse(
        # conditional clause
        class(models[[i]][[1]][[1]])[1] == "segmented",
        # TRUE: extract the slopes
        slopes[[i]]<- models[[i]]$result[[1]] %>%
            segmented::slope() %>%
            unlist() %>%
            enframe() %>%
            filter(name == "week1" | name == "week2") %>%
            pivot_wider(names_from = name, values_from = value) %>%
            rename(slope1 = week1, slope2 = week2) %>%
            mutate(diff = slope2 - slope1),
        # FALSE: return a data frame with NAs.
        slopes[[i]] <- tibble(slope1 = NA, slope2 = NA, diff = 0)
    )
}
toc() # 1.7 secs
# recover the p-values
tic()
p_values <- 1:length(models) %>%
    map(
        function(x){
            tibble(
                p_value = models[[x]][[1]][[2]]$p.value,
                break_point1 = models[[x]][[1]][[2]]$statistic, #from Davis test
                break_point2 = models[[x]][[1]][[1]]$psi[2],
                break_std_error = models[[x]][[1]][[1]]$psi[3]
            )
        }
    )
toc() # 0.8 secs

## J200916: Revise Muggeo's paper and see if the break point is best calculated on the David test or the regression. The regression provides a slightly different breaking point and has a St.Err that I can use on the graphics.
### recover results in a data frame:
slopes <- slopes %>%
    bind_rows() %>%
    mutate(lon = keys$lon, lat = keys$lat, stat = keys$stat)
p_values <- p_values %>%
    bind_rows() %>%
    mutate(lon = keys$lon, lat = keys$lat, stat = keys$stat)

px_results <- left_join(slopes, p_values) %>%
    mutate(break_pt = round(break_point2))

px_results <- px_results %>%
    left_join(
        df_pixel[[1]] %>% dplyr::select(time, week),
        by = c("break_pt" = "week"))

## I like this graph, maybe useful when done with all.
px_results %>%
    dplyr::select(slope1, slope2, diff, stat) %>%
    pivot_longer(cols = 1:2, names_to = "slopes", values_to = "value") %>%
    #filter(lon == lons[i], lat == lats[i]) %>%
    ggplot(aes(slopes, value)) +
    geom_point() +
    geom_boxplot() +
    facet_wrap(~stat, scales = "free")

px_results <- px_results %>%
    mutate(ews_type  = slope2 > slope1, up = slope2 > 0)

px_results %>% dplyr::select(1:3, stat, ews_type, up, lon, lat) %>%
    filter(stat != "ews_kur", stat != "ews_skw") %>%
    filter(up == FALSE)
    print(n = 21)

## it seems that all time series have jumps on the same spot:
df_pixel %>%
    mutate(pxid = paste(as.character(lon), as.character(lat), sep = "_")) %>%
    ggplot(aes(y = ews_fd,x= time, group = pxid)) +
    geom_line(size = 0.1) +
    facet_wrap(~pxid, scales = "free", ncol =1) +
    theme_light(base_size = 6)
## Not really, but the difference size is sensitive to the outliers. It might have been a fire or a shock event that affected a particular pixel, but as soon as it is not the only one event, the signal is reliable.
