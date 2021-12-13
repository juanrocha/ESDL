## Model-based EWS
## Juan Rocha
## 211007


library(tidyverse)
library(fs)
library(here)
library(slider)
library(tictoc)
library(fractaldim)
library(MARSS)
library(future)
library(furrr)
#### settings ####
# if working outside RStudio:
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL")
set.seed(9876)

fls <- dir_ls("Results/ews_halfwindow_terrestrial_ecosystem_respiration_log/")
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_terrestrial_ecosystem_respiration_log.RData')

#### Test for individual stats ####
## Repeat for each dataset (gpp, npp, cloA)
load("Results/210301_delta_detected_TER_log.RData")

## sample 100pxl
vars_ews <- names(df_delta_detected) |> str_subset("ews_delta")

samples <- df_delta_detected |>
    ungroup() |>
    filter(n_ews >= 3, !is.na(biome)) |> # >= 3
    slice_sample(n = 100)

## Get some raw data:
samples <- samples |>
    mutate(file = paste0("Results/ews_halfwindow_terrestrial_ecosystem_respiration_log/lat_", as.character(lat), ".csv"))

z <- samples |> pull(lat) |> unique() # vector of latitudes
dat <- list() # list holder for results

## read cleaned data for a few selected pixels:
tic()
for (i in seq_along(z)) {
    x <- samples |>
        filter(lat == z[i])
    w <- x |> pull(lon) |> unique() # vector of longitudes
    ## here is the line with the bug: I was reading the same file multiple times!
    y <- x$file %>% unique() %>% read_csv()
    y <- y |> filter(lon %in% w)
    y$lat <- z[i]
    dat[[i]] <- y |>
        select(time, lon, lat, gpp_1d) |>
        filter(!is.na(gpp_1d))
}
toc() #28s

dat <- bind_rows(dat)

dat <- dat |>
    mutate(group = paste(lon, lat, sep = "_")) |>
    split(~group)


## try Carpenter approach: doesn't work
source("Carpenter-DLM.R")
## wrap the function with safely so if there is errors when running several pixels it does not halt the whole computation.
safe_dlm <- safely(DLM.MLE)
out <- list()

## In parallel
plan(multicore, workers = 10)

tic()
out <- future_map(.x = dat, .f = safe_dlm, nl = 1,
    .options = furrr_options(seed = TRUE))  # safe_dlm(nl = 1, dat[[100]]) # for 1 pxl
toc() # 77mins on 100 pxls | 4hrs on non detected pixels

object.size(out) %>% format("Mb") # 5.5Mb

out <- transpose(out)

is_ok <- out$error %>% map(function(x) is.null(x)) %>% unlist()
all(is_ok) # all good

out <- out$result %>%
    bind_rows()

## useful summaries for writing
out |>
    mutate(col = case_when(
        #lamda-lamda.se > 1 ~ 1,
        #lamda > 1 & lamda-lamda.se <1 ~ 1,
        lamda > 1 ~ 1,
        TRUE ~ 0
    )) |>
    group_by(group) |>
    summarize(
        lamda_points = sum(col, na.rm = TRUE),
        max_lamda = max(lamda, na.rm = TRUE)) |>
    arrange(desc(lamda_points)) |>
    print(n = 100)

## Something strange: TER detected variables do not show lamda > 1 for any pixel. First I thought it was an error, I repeated the analysis on 100 random pixels with the same seed 9876 and then with a different 2021 seed (the second is the 211108_lambda_ter_detected2 file). Both are consistent in not finding high lambdas.

##
save(out,
     file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/211108_lambda_ter_detected2.RData")

#### Visualizations ####
out |>
  ggplot(aes(time, lamda, group = group)) +
  geom_path(alpha = 0.2, size = 0.2)

## one pxl
p3 <- out |>
    filter(group == "-59.125_13.125") |>
    mutate(col = case_when(
        lamda-lamda.se > 1 ~ 2,
        lamda > 1 & lamda-lamda.se <1 ~ 1,
        TRUE ~ 0
    )) |>
    ggplot(aes(time, lamda)) +
    geom_ribbon(aes(ymin = lamda-lamda.se, ymax = lamda+lamda.se), alpha = 0.3) +
    geom_line() +
    geom_point(aes(color = as.factor(col)), size = 0.3) +
    geom_hline(yintercept = 1)

out5 |>
    filter(lamda-lamda.se > 1)

## It is unrealistic to fit this at large scale. Even at medium res, there is
## 418,776 pixels in the marine dataset, it will take 437 days to run the computation
## at 90s per time series.

## try Dakos approach
tic()
out <- y |> filter(!is.na(gpp_1d)) |>
    pull(gpp_1d) |>
    ar.ols(demean = FALSE, intercep = FALSE)
toc() #0.03sec but not time varying
out
