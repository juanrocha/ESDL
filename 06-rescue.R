## This file re-start the calculation of EWS when the original function `early_warning` fails  half way. The original function takes an RData file with one latitude and all longitudes, calculate the generic critical slowing down based EWS, produce a csv file with results, and a summary dataframe for plotting. It worked well with gross primary productivity, but failed with other datasets, it ran out memory during the computation and crashed. So this files solves the problem, takes over from where the original attempt failed and ommit the summary dataframe for plotting (I can recover that later on a separate computation). This files follows the tempalte of `01-load_processed_data.R` but simplifying all that is not needed to avoid failure.

library(tidyverse)
library(tictoc)
library(future)
library(furrr)
library(tsibble)
library(progress)
library(fractaldim)


## J200812: Problem partially solved by removing the time series that were only zeroes.
## modified function:

early_warning <- function(x, window){
    # load the file, all objects are called mat
    load(x)
    ## make into a data frame
    tic()
    df <- as_tibble(mat) %>%
        mutate(time = time) %>%
        pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
        filter(!is.na(gpp))
    toc() # 0.2 secs

    if (dim(df)[1] > 0) {
        # shall I modify here `& any(df$gpp != 0)`?
        # filter out all zeroes ts:
        tic()
        df <- df %>%
            mutate(lon = as.numeric(lon)) %>%
            group_by(lon) %>%
            mutate(non_zeroes = any(gpp != 0)) %>%
            filter(non_zeroes == TRUE) %>%
            select(-non_zeroes)
        toc() # 0.4secs

        # calculate first difference
        tic()
        df <- df %>%
            mutate(
                gpp_1d = slide_dbl(gpp, diff, .size = 2)
            )
        toc() # 1.776 sec | 5 secs with root moisture -> improved to 1.4secs after zero removal.

        # add one column per each early warning
        tic()
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
                gpp_1d, function(x) abs(moments::skewness(x, na.rm = TRUE)), .size = window),
            ews_fd = slide_dbl(
                gpp_1d, function(x) fd.estimate(x, window.size = window, method = "madogram")$fd,  .size = window)
            )

        toc() ## 18 secs | 60secs root moisture (too many all zeroes), improved to 13 secs once 0s removed. 30secs including fractal dim.

        ## write results to file
        tic()
        write_csv(df,
             path = paste("~/Documents/Projects/ESDL_earlyadopter/ESDL/results_tmp/", str_replace(x, "RData", "csv"), sep = ""))
        toc() # 0.9 secs.
        ## summarize the result in timeless statistics:
        # tic()
        # df_results <- df %>%
        #     summarize(
        #         ## J200723: Add here some summary statistics of the original time series: variance and std of gpp (assumed to be 1)
        #         max_std = max(ews_std, na.rm = TRUE),
        #         min_std = min(ews_std, na.rm = TRUE),
        #         max_ac1 = max(ews_ac1, na.rm = TRUE),
        #         min_ac1 = min(ews_ac1, na.rm = TRUE),
        #         max_kur = max(ews_kur, na.rm = TRUE),
        #         min_kur = min(ews_kur, na.rm = TRUE),
        #         max_skw = max(ews_skw, na.rm = TRUE),
        #         min_skw = min(ews_skw, na.rm = TRUE)
        #         max_fd = max(ews_fd, na.rm = TRUE),
        #         min_fd = min(ews_fd, na.rm = TRUE)
        #     )
        # toc() # 0.61 sec
    }
    # ifelse(
    #     dim(df)[1] == 0,
    #     return(NA),
    #     return(df_results)
    # )
} # 19 secs in sequential

## load data and necessary keys:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_root_moisture.RData')
## original list of files from pre-processing in `Julia`
files <- list.files(
    path = "~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_root_moisture")
## files names to modify and compare from original list
files1 <- list.files(
    path = "~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_root_moisture")
## list of file names that suceeded before the crash.
files2 <- list.files(
    path = "~/Documents/Projects/ESDL_earlyadopter/ESDL/results_tmp")

files1 <- files1 %>% str_remove(".RData")
files2 <- files2 %>% str_remove(".csv")

is_ok <- files1 %in% files2 # these are the files missing from the first round.

# Window decides the window size: 52, 52*4, length(time)/2
window <- floor(length(time)/2)

## the new function does not return any value, except the file written to disk. So no need of return object, and instead of `future_map` then one uses `future_walk`
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_root_moisture")
# do it in parallel
plan(multisession, workers = 10)

tic()
files[!is_ok] %>%
    future_map(early_warning, window) # delete progress bar?
toc()
