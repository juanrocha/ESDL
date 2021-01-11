## Segmented regressions on early warning signals:
## Helps distinguishing critical slowing down from speeding up.
## J200917

library(tidyverse)
library(tictoc)
library(future)
library(furrr)
library(tsibble)

## Load cleaned data:
# setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_chlor_a")
# files <- list.files()
# load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_chlorA.RData')

# J200917: The segmented regression will be restricted to pixels where signals were detected. So load the detected dataset.

### J200920: Not working with chlorA, ran out of memory in sequential or parallel. I probably need to save files on the fly and collect results later. It took almost 1.5hrs before memory crash (but R didn't crashed)

key_var <- "terrestrial_ecosystem_respiration_log"

load(paste(
    "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201024_detected_", key_var, ".RData", sep = ""))

# load(
#     paste("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/200918_summary_", key_var, ".RData", sep = ""))

## select latitudes for files:
## read the raw data files for such lats
lats <- df_ews %>%
    pull(lat) %>%
    unique() %>%
    as.character()

## Change the path according to the data of use:
path <- paste(
    "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/ews_halfwindow_", key_var, "/", sep = "")


files <- paste(path, "lat_",lats, ".csv", sep = "")

### think carefully how to filter out unnecessary pixels, only the ones that match lon and lat in the df_ews dataset.

## Function modified to use value as response variable. The dataframe is transformed latter to long format where value corresponds to the numeric value of each early warning (stat)
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


# Reduce the search space, only apply the segmented regression on detected ews:
# J200920: Don't chose columns with numbers or ranges, it introduces errors. Use names, all of them, to avoid weird behaviour between files.
tic()
df_detected <- df_ews %>%
    dplyr::select(lon,lat, std,ac1,kur,skw,fd) %>%
    pivot_longer(cols = c(std,ac1,kur,skw,fd),
    names_to = "stat", values_to = "detect") %>%
    filter (detect == TRUE)
toc()

# test line to check and debug individual errors when they appear :
x <- files[100]
y <- as.numeric(lats[100])

multi_seggy <- function(x, y){
    # x is the file name, y is the latitude vector
    # Takes a file per latitude and extracts relevant lon pixels.
    #tic()
    dat <- read_csv(
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
    #toc() # 7 secs
    # extract the relevant longitudes:
    # tic()
    lons <- df_ews %>%
        filter(lat == y) %>%
        pull(lon)
    # toc() #0.3s
    # Now filter only the relevant pixels from the file:
    # tic()
    dat <- dat %>%
        filter(lon %in% lons)
    # toc() #0.1s
    ## Remember: there is an error on the fractal dimension, the first value is always zero. Remove it.
    dat$ews_fd[dat$ews_fd == 0] <- NA
    dat$lat <- y
    ## Make into a longer dataframe:
    dat <- dat %>%
        filter(!is.na(ews_fd)) %>%
        group_by(lon, lat) %>%
        mutate(week = row_number()) %>%
        dplyr::select(-gpp, -gpp_1d) %>%
        pivot_longer(cols = starts_with("ews"), names_to = "stat", values_to = "value")
    ## Reduce search space with detected dataframe
    # tic()
    dat <- dat %>%
        mutate(stat = str_remove_all(stat, "ews_")) %>%
        inner_join(df_detected)
    # toc()
    #toc() # 5 secs
    # prepare keys for later recover
    # tic()
    keys <- dat %>%
        group_by(lon, lat, stat) %>%
        group_keys()
    # split by lon
    dat <- dat %>%
        group_by(lon, lat, stat) %>%
        group_split()
    # toc() # 0.3s
    ## Apply seggy to all pixels
    # tic()
    models <- dat %>% map(seggy_safe)
    # toc() # 105secs = 1575 segmented regressions
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
    # toc() # 1.7 secs
    # recover the p-values
    # J200921: modification of p_value recovery to account for the case when the segmented regression does not coverge.
    # tic()
    p_values <- list()
    for (i in 1:length(models)){
        ifelse(
            # conditional clause
            class(models[[i]][[1]][[1]])[1] == "segmented",
            ## TRUE, it converged, there is a segmented regression objct
            p_values[[i]] <- tibble(
                p_value = models[[i]][[1]][[2]]$p.value,
                break_point1 = models[[i]][[1]][[2]]$statistic, #from Davis test
                break_point2 = models[[i]][[1]][[1]]$psi[2],
                break_std_error = models[[i]][[1]][[1]]$psi[3]
            ),
            ## FALSE, there is no break point.
            p_values[[i]] <- tibble(
                p_value = models[[i]][[1]][[2]]$p.value,
                break_point1 = models[[i]][[1]][[2]]$statistic, #from Davis test
                break_point2 = NA,
                break_std_error = NA
            )
        )
    }

    # toc() # 0.8 secs

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
            dat[[1]] %>% dplyr::select(time, week),
            by = c("break_pt" = "week"))

    return(px_results) # detected pixel results
}

multi_seggy_safe <- safely(multi_seggy)

## test:
tic()
test <- multi_seggy_safe(x,y)
toc() # 1.25s one latitude file with all its longitudes :)

plan(multicore, workers = 10) # do it in parallel
## For real:
results <- list()
tic()
results <- future_map2(files, as.numeric(lats), multi_seggy_safe)
toc() # 18 min GPP, 46mins terrestiral respiration in parallel, 76mins ChlorA.
# 45min lai-log
object.size(results) %>% format("Mb") # 125Mb

results <- transpose(results)

## skip errors (which are pixesl with only missing values)
is_ok <- results$error %>% map(function(x) is.null(x)) %>% unlist()

which(!is_ok) %>% length()
# multi_seggy_safe(files[208], as.numeric(lats[208]))
#
# #Fix errors
results2 <- list()
results2 <- future_map2(
    files[!is_ok], as.numeric(lats)[!is_ok],
    multi_seggy_safe)







px_results <- results[[1]] %>%
    bind_rows()

## save
save(px_results, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201028_segmented_terrestrial_ecosystem_respiration_log.RData")

## Some visualizations:
## create map
world <- ggplot(
    map_data("world") %>%
        rename(lon = long),
    aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
               size = 0.01) + # fill = "#f9f9f9",
    coord_quickmap() + theme_void()


px_results %>%
    ggplot(aes(time, lat)) +
    geom_density2d()

px_results %>%
    ggplot(aes(lon, time)) +
    geom_density2d()


px_results %>% #filter(stat == "fd") %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = time)) +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5)) +
    theme_void() + facet_wrap(~stat) +
    theme(legend.position = "bottom")

px_results %>%
    ggplot(aes(log(p_value))) + geom_density() +
    geom_vline(xintercept = log(0.01))

## I like this graph, maybe useful when done with all.
px_results %>%
    mutate(pxid = paste(as.character(lon), as.character(lat), sep = "_")) %>%
    #dplyr::select(slope1, slope2, diff, stat) %>%
    pivot_longer(cols = 1:2, names_to = "slopes", values_to = "value") %>%
    filter(stat != "kur", stat != "skw") %>%
    ggplot(aes(slopes, value, group = pxid)) +
    geom_line(aes(color = diff), alpha = 0.5, size = 0.1) +
    scale_color_gradient2("Slope difference", mid = "gray85",
                          guide = guide_colourbar(barwidth = 15, barheight = 0.5))+
    facet_wrap(~stat, scales = "free") +
    theme_light(base_size = 8) +
    theme(legend.position = "bottom")

# quartz.save(file = "figures/figS4_slopes_diff_GPP.png", type = "png",
#             width = 5, height = 3, dpi = 800)

px_results %>%
    mutate(ews_type  = slope2 > slope1, up = slope2 > 0) %>%
    dplyr::select(lon, lat, up, stat) %>%
    pivot_wider(names_from = stat, values_from = up) %>%
    mutate(
        csd = ifelse((ac1 == TRUE & std == TRUE), TRUE, NA ),
        csu = ifelse((ac1 == FALSE & std == FALSE), TRUE, NA),
        ksk = ifelse((kur == TRUE & skw == TRUE | kur == FALSE & skw == FALSE), TRUE, NA),
        ksk_amb = ifelse(kur == TRUE & skw == FALSE | kur == FALSE & skw == TRUE, TRUE, NA),
        amb = ifelse(ac1 == TRUE & std == FALSE | ac1 == FALSE & std == TRUE, TRUE, NA)
    ) %>% #rowwise() %>%
    mutate(
        other = ifelse(is.na(csd) & is.na(csu) & is.na(ksk) & is.na(ksk_amb) & is.na(amb), TRUE, NA)
    ) %>%
    summarize(
        csd = sum(csd, na.rm = TRUE),
        csu = sum(csu, na.rm = TRUE),
        ksk = sum(ksk, na.rm = TRUE),
        ksk_amb = sum(ksk_amb, na.rm = TRUE),
        amb = sum(amb, na.rm = TRUE),
        other = sum(other, na.rm = TRUE)
    )

df_ews$n_ews %>% table()

64484 - 38638 # number of pixels with more than one signal

px_results %>%
    #mutate(ews_type  = slope2 > slope1, up = slope2 > 0) %>%
    dplyr::select(lon, lat,  stat) %>%
    pivot_wider(names_from = stat, values_from = up)

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

#### graph diagnosis
dat %>%
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
    # geom_segment(
    #     data = annotation_df,
    #     aes(x = x_min, y = y_min, xend = x_max, yend = y_max),
    #     color = "dodgerblue", size = 0.5) +
    # geom_text(data = annotation_df, aes(x,y, label = label),
    #     parse = TRUE, size = 3, color = "dodgerblue",
    #     hjust = c(0.5, rep(1,5))) +
    # # This chuck add the break point result from the segmented regression.
    # geom_point(
    #     data = px_results %>%
    #         filter(lon == lons[i], lat == lats[i]) %>%
    #         mutate(value = c(0.2,1.3,5,0.5,0.1)) %>%
    #         mutate(stat = as_factor(stat),
    #             stat = fct_recode(stat,
    #             "Standard deviation" = "ews_std",
    #             "Autocorrelation lag-1" = "ews_ac1",
    #             "Kurtosis" = "ews_kur", "Skewness" = "ews_skw",
    #             "Fractal dimension" = "ews_fd")),
    #     aes(time, value)
    # ) +
    facet_wrap(vars(stat), scales = "free_y", nrow=6) +
    theme_light(base_size = 6)
