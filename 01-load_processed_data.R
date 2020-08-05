## Load processed data:
## J200704

library(tidyverse)
library(tictoc)
library(future)
library(furrr)
library(tsibble)

## Data folders are:
# /processed_gpp
# /processed_chlor_a
# /processed_leaf_area_index

setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_leaf_area_index")
files <- list.files()

#load(files[2])
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_LAI.RData')
# key files are:
# /keys_gpp.RData
# /keys_chlorA.RData
# /keys_LAI.RData

early_warning <- function(x, window){
    # load the file, all objects are called mat
    load(x)
    ## make into a data frame
    #tic()
    df <- as_tibble(mat) %>%
        mutate(time = time) %>%
        pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
        filter(!is.na(gpp))
    #toc() # 0.2 secs

    if (dim(df)[1] > 0) {
        # calculate first difference
        #tic()
        df <- df %>%
            mutate(lon = as.numeric(lon)) %>%
            group_by(lon) %>%
            mutate(
                gpp_1d = slide_dbl(gpp, diff, .size = 2)
            )
        #toc() # 1.776 sec

        # add one column per each early warning
        #tic()
        df <- df %>%
        mutate(
            ews_std = slide_dbl(
                gpp_1d, sd, na.rm = TRUE, .size = window),
            ews_ac1 = slide_dbl(
                gpp_1d,
                function(x) cor(x,lag(x,1), use = "pairwise.complete.obs", "pearson"),  .size = window),
            ews_kur = slide_dbl(
                gpp_1d, moments::kurtosis, na.rm = TRUE, .size = window),
            ews_skw = slide_dbl(
                gpp_1d, function(x) abs(moments::skewness(x, na.rm = TRUE)), .size = window)
        )
        #toc() ## 18 secs

        ## write results to file
        #tic()
        write_csv(df,
             path = paste("~/Documents/Projects/ESDL_earlyadopter/ESDL/results_tmp/", str_replace(x, "RData", "csv"), sep = ""))
        #toc()
        ## summarize the result in timeless statistics:
        #tic()
        df_results <- df %>%
            summarize(
                ## J200723: Add here some summary statistics of the original time series: variance and std of gpp (assumed to be 1)
                max_std = max(ews_std, na.rm = TRUE),
                min_std = min(ews_std, na.rm = TRUE),
                max_ac1 = max(ews_ac1, na.rm = TRUE),
                min_ac1 = min(ews_ac1, na.rm = TRUE),
                max_kur = max(ews_kur, na.rm = TRUE),
                min_kur = min(ews_kur, na.rm = TRUE),
                max_skw = max(ews_skw, na.rm = TRUE),
                min_skw = min(ews_skw, na.rm = TRUE),
            )
        #toc() # 0.32 sec
    }
    ifelse(
        dim(df)[1] == 0,
        return(NA),
        return(df_results)
    )
} # 19 secs in sequential

# Window decides the window size: 52, 52*4, length(time)/2
window <- length(time)/2
# do it in parallel
plan(multicore, workers = 10)
tic()
results <- files %>%
    future_map(early_warning, window, .progress = TRUE)
toc() # 60 mins terrestrial data, 2.15 hours marine

## J200805: `early_warning` finished in the LAI data  but results were not stored, it ran out memory. I can recovered however from the files produced later, perhaps with sparklyr.

object.size(results) %>% format("Mb") # 3.9Gb in RAM, cannot do that on high res

# Not alll results are ok, some are null values, identify them:
not_ok <- is.na(results)

# latitudes are wrong, recover them from file names
lat <- files %>% str_remove("lat_") %>% str_remove(".RData") %>% as.numeric()
## Add corrected latitudes
tic()
results <- results[!not_ok] %>%
    map2(., lat[!not_ok], function(x,y) x %>% add_column(lat = y))
toc() # 3.5 secs

## Note that the latitudes that do not have data were dropped, so only 543 slices of data are preserved, all with lon and lat coords.
length(results) # 543

# save(results, file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/200805_results_ews_halfwindow_chlorA.RData")

# visualize some diagnostics
## To-do: calculate the differences for all ews, panel map comparing ews + distributions. If all have long tails, one can just continue the analysis of segmented regressions on the tails. Plot the quantiles.
df <- results %>%
    bind_rows() %>%
    group_by(lon, lat) %>%
    mutate(
        diff_std = max_std - min_std,
        diff_ac1 = max_ac1 - min_ac1,
        diff_kur = max_kur - min_kur,
        diff_skw = max_skw - min_skw
    ) %>%
    select(lon, lat, starts_with("diff")) %>%
    pivot_longer(cols = 3:6,
        names_to = c(".value", "stat"),
        names_sep = "_"
    )

df %>%  ggplot(aes(diff_std)) + geom_density()
    #maps
df %>% filter(stat == "skw") %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = diff)) +
    scale_fill_viridis_c() +
    #facet_wrap(~stat) +
    theme_light()


#### Old code ####
## Previous attempts here archived
## 200805

## make a dataframe and calculate the first difference of gpp to get rid of additional sesonal variation (some pixels have when checking the acf plot.)
# tic()
# results <- results %>%
#     bind_rows() %>%
#     mutate(lon = as.numeric(lon)) %>%
#     group_by(lon, lat) %>%
#     mutate(gpp_1d = slide_dbl(gpp, diff, .size = 2)) %>%
#     select(-gpp) %>%
#     rename(gpp = gpp_1d)
# toc() # 16min sequential

# ## Converting all to df make it very slow because I cannot use future_map. Keep it as a list and do it on parallel
# plan(multicore, workers = 10)
# future.options(future.globals.maxSize = 700 * 1024 ^ 2)
# tic()
# test <- results %>%
#     #head() %>% # for testing and benchmarking 4.3 secs
#     future_map(~ . %>%
#             mutate(lon = as.numeric(lon),
#             gpp_1d = slide_dbl(gpp, diff, .size = 2)
#         ) %>%
#         select(-gpp) %>%
#         rename(gpp = gpp_1d) %>%
#         filter(!is.na(gpp)),
#     globals = list(future.globals.maxSize = ))
# toc() ## 11secs


## Calculate early warnings:
# do it in parallel
# plan(multicore, workers = 10)

# variable <- enquo(gpp)
# J200720: `mutate` does not do the computation in parallel.

# tic()
# results <- results %>%
#     mutate(
#         ews_std = slide_dbl(gpp, sd, na.rm = TRUE, .size = window),
#         ews_ac1 = slide_dbl(gpp,
#             function(x) cor(x,lag(x,1), use = "pairwise.complete.obs", "spearman"),  .size = window),
#         ews_kur = slide_dbl(
#             gpp, moments::kurtosis, na.rm = TRUE, .size = window),
#         ews_skw = slide_dbl(
#             gpp, function(x) abs(moments::skewness(x, na.rm = TRUE)), .size = window)
#     ) %>%
#     mutate(key = row_number())
# toc() # 1.4hrs

# tic()
# df <- results %>% filter(!is.na(std_half))
# toc() # 14 sec


### This steps are worth running later
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

world <-map_data("world") %>%
    rename(lon = long) %>%
    ggplot(aes(lon, lat, group = group))+
    geom_polygon(color = "white", size = 0.01)


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


## J200720: Testing for AR1,2

x <- df %>% filter(lon == -91.625) %>% pull(gpp)
forecast::ndiffs(x) # 0
urca::ur.kpss(x, lags = "long", use.lag = 0)
acf(x)

tic()
ar1 <- ar.ols(x[1:52*4], aic = FALSE, order.max = 2, dmean = FALSE, intercept = FALSE)
toc() # 0.045secs

tic()
ar2 <- acf(x[1:52*4], type = c("correlation"), plot = TRUE)
toc()

test <- df %>% filter(lon == -91.625)

test <- test %>%
    mutate(gpp_1d = slide_dbl(gpp, diff, .size = 2)) %>%
    mutate(lag1 = lag(gpp_1d, n = 1), lag1 = lag(gpp_1d, 2))

tic()
test1 <- test %>%
    mutate(ac1 = slide2(gpp_1d, lag1, function(x,y) cor(x,y, use = "pairwise.complete.obs", method = "spearman"), .size = 52*4)) %>%
    unnest(cols = c(ac1))
toc() #0.448 secs, 0.2secs with pearson and spearman.

tic()
test2 <- test %>%
    filter(!is.na(gpp_1d)) %>%
    mutate(ac1 = slide_dbl(
        gpp_1d, function(x) {
            ar <- ar.ols(x, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
            return(ar$ar)
        }, .size = 52*4))
toc() # 0.618 secs


test1[205:225,]

tic()
cor(x,lag(x,1), use = "pairwise.complete.obs", "spearman")
toc()

tic()
ar.ols(x, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar
toc()
