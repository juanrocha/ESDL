## calculate the EWS from R directly
## J200703

library(tidyverse)
library(tictoc)
library(future)
library(furrr)
library(tsibble)

## Load cleaned data:
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_gpp")
files <- list.files()

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_gpp.RData')

seggy <- function(x){
    dfr <- x %>%
        filter(!is.na(ew_std_4year)) # get rid of missing values
    fit <- lm(ew_std_half ~ week, data = dfr)
    # no predefined psi (break point)
    sfit <- segmented::segmented(fit, ~ week)
    # no predefined k (number of points to evaluate hypothesis)
    davies <- segmented::davies.test(fit , ~ week, k = 10)
    return(list(sfit, davies))
}

seggy_safe <- safely(seggy)



multi_seggy <- function(x){  # x is the file name
    #tic()
    # Takes a file per latitude and run the analysis per each longitud pixel.
    load(x)                 # load the file, all objects are called mat
    ## make into a data frame
    df <- as_tibble(mat) %>%
        mutate(time = time) %>%
        pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
        filter(!is.na(gpp))
    ## calculate std
    #tic()
    dat <- df %>%
        group_by(lon) %>%
        mutate(
            ew_std_4year = slide_dbl(gpp, sd, na.rm = TRUE, .size = 52*4),
            ew_std_half = slide_dbl(gpp, sd, na.rm = TRUE, .size = length(time)/2)
        ) %>%
        mutate(week = row_number())
    #toc() # 5 secs
    # prepare keys for later recover
    #tic()
    keys <- dat %>%
        group_keys()
    # split by lon
    dat <- dat %>%
        group_split()
    #toc()
    ## Apply seggy to all pixels
    #tic()
    models <- dat %>% future_map(seggy_safe, .progress = TRUE)
    #toc() # 20 sec in parallel
    ## extract slopes:
    slopes <- list()
    #tic()
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
    #toc() # 5 secs
    # recover the p-values
    #tic()
    p_values <- 1:length(models) %>%
        future_map(
            function(x){
                tibble(
                    p_value = models[[x]][[1]][[2]]$p.value,
                    break_point = models[[x]][[1]][[2]]$statistic
                )
            }
        )
    #toc() # 1.6 secs
    ### recover results in a data frame:
    slopes <- slopes %>%
        bind_rows() %>%
        mutate(lon = keys$lon)
    p_values <- p_values %>%
        bind_rows() %>%
        mutate(lon = keys$lon)

    df_results <- left_join(slopes, p_values) %>%
        mutate(key = row_number())
    ## the key here is the number of the pixel (lon) with respect to each file (lat). In case I need to go back, I can use the indexing to re-run the analysis on one particular pixe.
    #toc()
    ifelse(
        dim(df)[1] == 0,
        return(NA),
        return(df_results)
    )

}

multi_seggy_safe <- safely(multi_seggy)


plan(multicore, workers = 10) # do it in parallel
#### Execute on a test:
# tic()
# test1 <- multi_seggy_safe(files[1])
# toc() # 14 secs when there is no error.
## It should take: 14 sec * 720 files / 60 min / 60 hrs = 2.8hrs
## For real:
tic()
results <- files %>%
    future_map(multi_seggy_safe)
toc() # 6000.922 sec ~ 1.5 hrs.

object.size(results) %>% format("Mb") # 125Mb

results <- transpose(results)

## skip errors (which are pixesl with only missing values)
is_ok <- results$error %>% map(function(x) is.null(x)) %>% unlist()

# latitudes are wrong, recover them from file names
lat <- files %>% str_remove("lat_") %>% str_remove(".RData") %>% as.numeric()


## Adding latitude
results$result[which(is_ok)] <- results$result[which(is_ok)] %>%
    map2(., lat[which(is_ok)], function(x,y) x %>% add_column(lat = y))

df_results <- results[[1]] %>%
    bind_rows() %>%
    mutate(lon = as.numeric(lon)) %>%
    mutate(time = time[break_point])

## save
setwd("/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/")
save(df_results, file = "200703_std_gpp_results.RData")
## Some visualizations:
## create map
world <- ggplot(
    map_data("world") %>%
        rename(lon = long),
    aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
               size = 0.01) + # fill = "#f9f9f9",
    coord_quickmap() + theme_void()


df_results %>%
    ggplot(aes(time, lat)) +
    geom_density2d()

df_results %>%
    ggplot(aes(lon, time)) +
    geom_density2d()


df_results %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = slope2)) +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5)) +
    theme_void() +
    theme(legend.position = "bottom")









## Notes:
# * Calculating the AR coefficient is computationally expensive. Takes way too long and the signal
# * I'm not sure I should use future_map twice. Now it appears once within the function and then when I run for the full batch. Probably I lost parallel power because I'm calling parallel within parallel. It's better only once with clear

# tic()
# df %>%
#     group_by(lon) %>%
#     mutate(gpp_lag = lag(gpp, n = 1)) %>%
#     mutate(
#         # ew_std_year = slide_dbl(gpp, sd, na.rm = TRUE, .size = 52),
#         ew_std_4year = slide_dbl(gpp, sd, na.rm = TRUE, .size = 52*4),
#         ew_std_half = slide_dbl(gpp, sd, na.rm = TRUE, .size = length(time)/2),
#         #ew_ar_year = slide2_dbl(gpp, gpp_lag, cor, .fill = NA, method = "kendall", .size = 52),
#         ew_ar_4year = slide2_dbl(gpp, gpp_lag, cor, .fill = NA, method = "kendall", .size = 52*4),
#         ew_ar_half = slide2_dbl(gpp, gpp_lag, cor, .fill = NA, method = "kendall", .size = length(time)/2)
#     )
# toc() # 5 secs non-parallel! when calculaing std, 334.362 sec when calculating AR coef also. Too much, over 5min per latitude file.

## visualize one pixel:
# df %>% filter(lon == "-91.625")
df[[6]] %>%
    pivot_longer(.,
        cols = starts_with("ew_"),
        names_to = "ews_window",
        values_to = "ews_value") %>%
    ggplot(aes(time, ews_value)) +
    geom_line(aes(color = ews_window), size = 0.5) +
    facet_wrap(ews_window~., ncol = 1,  scales = "free_y")
