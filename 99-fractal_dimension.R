## Fractal dimension analysis
## 200713
## juan.rocha@su.se

## Load processed data:
## J200704

library(tidyverse)
library(tictoc)
library(future)
library(furrr)
library(tsibble)
library(fractaldim)


setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/results_ews_halfwindow_lai")
files <- list.files()

#load(files[2])
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_LAI.RData')


fractal <- function(x){ # x is the file name
    ## read csv
    #tic()
    df <- read_csv(
        file = x,
        col_types = cols(
            time = col_datetime(),
            lon = col_double(),
            gpp = col_double(),
            gpp_1d = col_double(),
            ews_std = col_double(),
            ews_ac1 = col_double(),
            ews_kur = col_double(),
            ews_skw = col_double()
        )
    )
    #toc() # 0.8 secs

    #tic()
    df <- df %>%
        group_by(lon) %>%
        mutate(
            ews_fd = slide_dbl(
                gpp_1d,
                function(x) fd.estimate(x, window.size = length(time)/2, method = "madogram")$fd,  .size = length(time)/2)
        )
    #toc() # will take 10.6 hrs. 53secs * 720 files / 60 / 60 on single threat for only one fd calculation.
    ### 20 secs when calculating only one window size :)

    write_csv(df,
         path = paste("~/Documents/Projects/ESDL_earlyadopter/ESDL/results_tmp/", x, sep = ""))

    rm(df) # trying to save memory
    # return(df_results)

}

fractal_safe <- safely(fractal)


plan(multicore, workers = 10) # do it in parallel
#### Execute on a test:
tic()
test1 <- fractal(files[1])  ## don't assign the result
toc() # 47 secs when there is no error. 101 secs for a 120Mb file of ChlorA.
# If you assign the results, it get's saved in RAM. Maybe a way to speed the process and avoid overloading the RAM is not assigning the result, just intersted on the side effects of the function (which includes saving it as csv). -- 9.33 hrs in sequential for LAI data.

## recovery mode:
# files2 <- list.files("~/Documents/Projects/ESDL_earlyadopter/ESDL/results_tmp")
# is_ok <- files %in% files2

## For real:
tic()
files %>%
    future_map(fractal)
toc() # New improvement 1.4hrs GPP data. Took 8.3 hours in sequential,
# 4.3 hrs in parallel for ChlorA data. Using fractal_safe the job got interrupted at the end and results lost. I'm not sure if the problem is related to `future_map` or the `safely` option of `fractal`. The error I got is: The process has forked and you cannot use this CoreFoundation functionality safely. You MUST exec().
# Break on __THE_PROCESS_HAS_FORKED_AND_YOU_CANNOT_USE_THIS_COREFOUNDATION_FUNCTIONALITY___YOU_MUST_EXEC__() to debug.

object.size(results) %>% format("Gb") # 125Mb

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
    ungroup() %>%
    mutate(lon = as.numeric(lon))

object.size(df_results) %>% format("Gb") # 125Mb

## save
# setwd("/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/")
# save(df_fractaldim, file = "200714_fd_std_gpp_results.RData")


# J200714: Idea! add a first diff on the fractal dimension on the df_results (all time), and in the summary add a # of increases / decreases, similar to:
tic()
df_fractaldim <- df_results %>%
    select(-fd_std_half) %>%
    mutate(
        first_diff = slide_dbl(fd_std_4year, diff, .size = 2),
        increased = first_diff > 0
    ) %>%
    group_by(lat, lon) %>%
    summarize(
        increase = sum(increased, na.rm = TRUE),
        decrease = sum(!increased, na.rm = TRUE),
        drift = sum(first_diff, na.rm = TRUE),
        fd_min = min(fd_std_4year, na.rm = TRUE),
        fd_max = max(fd_std_4year, na.rm = TRUE),
        fd_mean = mean(fd_std_4year, na.rm = TRUE),
        fd_diff = fd_max - fd_min)
toc() # 16min


test %>%
    select(-fd_diff) %>%
    pivot_longer(
        cols = starts_with("fd"),
        names_to = "fractal_dim",
        values_to = "value") %>%
        ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c() +
    facet_wrap(~fractal_dim, ncol = 2) +
    theme_light()

test %>%
    # mutate(ratio = increase / decrease) %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = drift)) +
    scale_fill_viridis_c(direction = -1) +
    theme_light()

test %>%
    pivot_longer(
        cols = starts_with("fd"),
        names_to = "fractal_dim",
        values_to = "value") %>%
    ggplot(aes(value)) +
    geom_density(aes(fill = fractal_dim)) +
    facet_wrap(~fractal_dim, ncol = 2) +
    theme_light()



### deprecated function:
#
# fractal_scratch <- function(x){ # x is the file name
#     load(x)             # load the file, all objects are called mat
#     ## make into a data frame
#     df <- as_tibble(mat) %>%
#         mutate(time = time) %>%
#         pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
#         filter(!is.na(gpp)) ## remove all time series where all values are NAs.
#         # remember the timeseries was clean and filled gaps in Julia, so NA are for pixels with no obs over time.
#
#     #tic()
#     df_results <- df %>%       # transform the df into a list by longitud
#         group_by(lon) %>%
#         mutate(
#             fd_std_4year = slide_dbl(
#                 gpp,
#                 function(x) fd.estimate(x, window.size = 52*4, method = "madogram")$fd,  .size = 52*4),
#             fd_std_half = slide_dbl(
#                 gpp,
#                 function(x) fd.estimate(x, window.size = length(time)/2, method = "madogram")$fd,  .size = length(time)/2)
#         )
#     #toc() # will take 10.6 hrs. 53secs * 720 files / 60 / 60 on single threat for only one fd calculation.
#
#     ifelse(
#         dim(df)[1] == 0,
#         return(NA),
#         return(df_results)
#     )
# }
