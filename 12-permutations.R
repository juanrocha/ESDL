## Null models by permutation
## Juan Rocha
## 211007

library(tidyverse)
library(fs)
library(here)
library(slider)
library(tictoc)
library(fractaldim)

#### settings ####
# if working outside RStudio:
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL")

fls <- dir_ls("Results/ews_halfwindow_chlorA_log/")
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_chlorA_log.RData')

#### Test for individual stats ####
## Repeat for each dataset (gpp, npp, cloA)
load("Results/210301_delta_detected_chlorA_log.RData")

## sample 100pxl
vars_ews <- names(df_delta_detected) |> str_subset("ews_delta")

samples <- map(
    vars_ews,
    function(x) {
        s <- df_delta_detected |>
            ungroup() |>
            filter(eval(sym(x)) == TRUE, !is.na(biome)) |>
            slice_sample(n = 500)
        return(s)
    }
)

## This is the number of pixels detected, remember it is 10% of the full planet (5% of the distribution on each side).
df_delta_detected |>
    ungroup() |>
    select(starts_with("ews_delta")) |>
    colSums()

samples <- samples |>
    map(function(x) {
        x <- x |>
    ## change here the path every time you change datasets
        mutate(file = paste0("Results/ews_halfwindow_chlorA_log/lat_", as.character(lat), ".csv"))
        return(x)})

# add the key variable to recover stats later:
samples <-  map2(samples, vars_ews, function (x,y) {x$key_var <- y; return(x)})

samples <- bind_rows(samples)

## J211020: this is inneficient because it has to read as many files as pixels.
## A more efficient way would be to read the file only once, and keep the lons (if multiple)
z <- samples |> pull(lat) |> unique() # vector of latitudes
dat <- list() # list holder for results

## J211101: I crash R anytime I try to scale up to more than 100pxls at the time
# In previous exercises I do calculations, retreive results, and never have too
# much data on memory. Maybe need to do the same here, seems that 500pxls per metric is too much.

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
toc() # 80s for 100pxl each (500) | 351s and >674Mb on memory

# dat <- map2(
#     .x = samples$file, .y = samples$lon,
#     .f = function(x,y) {
#         z <- read_csv(file = x)
#         z <- z |> filter(lon == y)
#         return(z)
#     })
#
# dat <- map2(
#     .x = dat,
#     .y = samples$lat,
#     function(x, y) {x$lat <- y; return(x)})

dat <- bind_rows(dat)
## with 2500pxls it becomes 22M obs, why?


dat <- dat |>
    group_by(lon,lat, time) |>
    unique() |>
    ungroup()

dat |> group_by(lon,lat) |>
    summarize(obs = n()) # does reduce to 827obs per pixel
# I had the problem that many pixels were sampled repeatedly because more than one EWS detect them. Now it should work again.

##Jumpt to line with permutations###


#### Test on pixels with multiple detection ####
sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_delta_TER.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        #biome_code = col_double(),
        biome = col_character(),
        n_ews = col_double()
    ))

sample |>
    group_by(n_ews) |>
    dplyr::summarize(n()) # max 47, balance the sample on the lower class

subsample <- sample |>
    group_by(n_ews) |>
    slice_sample(n = 47)


subsample <- subsample |>
    arrange(lat) |>
    mutate(file = paste0("Results/ews_halfwindow_gpp_log/lat_", as.character(lat), ".csv"))

subsample$file %in% fls


dat <- map2(
    .x = subsample$file, .y = subsample$lon,
    .f = function(x,y) {
        z <- read_csv(file = x)
        z <- z |> filter(lon == y)
        return(z)
    })

dat <- map2(
    .x = dat,
    .y = subsample$lat,
    function(x, y) {x$lat <- y; return(x)})

dat <- bind_rows(dat) |>
    select(time, lon, lat, gpp_1d) |>
    filter(!is.na(gpp_1d))

#### permutations ####
prm <- dat |>
    group_by(lon,lat) |>
    modelr::permute(n = 10, gpp_1d)

# Window decides the window size: 52, 52*4, length(time)/2
w <- floor(length(time-1)/2)
x <- dat |> filter(lon == -63.625, lat == -35.875) # dataset for testing
## design a function to extract the stats of interest: delta of std
## J211026: Fractal dimension is not working, returns all zeroes without errors
#### Solving fractal dimension problems ####
# x |>
#     group_by(lon,lat) |>
#     mutate(fd = slide_dbl(
#         gpp_1d,
#         function(x) {
#             z <- fd.estimate(x, window.size = w, method = "madogram")
#             return(as.vector(z$fd))} , .before = w, .after = 0, .complete = TRUE)) |>
#     ggplot(aes(time,fd)) + geom_line()
#
# out <- fd.estimate(data = x$gpp_1d, window.size = w, method = "madogram")
## I think I found the bug, missing the window on the .before = w argument

####
delta <- function(x) {
    # x is a dataframe with lon, lat, time and gpp_1d
    x |>
        group_by(lon, lat) |>
        mutate(ews_std = slider::slide_dbl(
            gpp_1d, sd, na.rm = TRUE, .before = w, .after = 0, .complete = TRUE),
            ews_ac1 = slide_dbl(
                gpp_1d,
                function(x) cor(x,lag(x,1), use = "pairwise.complete.obs", "pearson"),
                .before = w, .after = 0, .complete = TRUE),
            ews_kur = slide_dbl(
                gpp_1d, moments::kurtosis, na.rm = TRUE, .before = w,
                .after = 0, .complete = TRUE),
            ews_skw = slide_dbl(
                gpp_1d, function(x) abs(moments::skewness(x, na.rm = TRUE)),
                .before = w, .after = 0, .complete = TRUE) ,
            # calculating fractal dimension here doubles the time but it's done.
            ews_fd = slide_dbl(
                gpp_1d,
                function(x) {
                    z <- fd.estimate(x, window.size = w, method = "madogram")$fd
                    return(as.vector(z))} , .before = w, .after = 0, .complete = TRUE)) |>
        pivot_longer(cols = starts_with("ews"), names_to = "ews", values_to = "value") %>%
        filter(!is.na(value)) |>
        group_by(lat, lon, ews) %>%
        filter(value == max(value) | value == min(value)) %>%
        arrange(time) |>
        summarize(
            min = min(value),
            max = max(value),
            delta = diff(value),
            abruptness = diff(time)
        ) %>%  # the following step is necessary because there is non-unique values
        # on the fractal dimension. Many datapoints that meet the condition min-max.
        filter(abruptness == min(abruptness)) %>%
        unique() %>%  ungroup()
}

tic()
real <- delta(dat)
toc()
#0.039s for one pixel, dat has 282 pxls ~ 10s in dat... each replica has 10 perms of dat
## 196 s with all ews calculated... fractal dimension doesn't seem to be working correctly (all zeroes)
## 7min for dat with 500 pixels in it.

test_all <- list()
## I need a for loop because the lenght of elements is not the same (error when using map)
tic()
for (i in seq_along(prm$perm)) {
    test_all[[i]] <- delta(dat[prm$perm[[i]]$idx ,])
}
toc() #16secs for std only, 5902s with all EWS; 3830 in 500px sample


test_all <- map2(test_all, prm$.id, function(x,y) {x$perm <- y; return(x)})

real$perm <- "real"

test_all <- bind_rows(test_all)
test_all <- bind_rows(test_all, real)

## Visualizations for each ews individually:
test_all


## Visualizations for 5 EWS:
test_all |>
    left_join(select(samples, lon, lat, n_ews, biome)) |>
    mutate(abs_delta = abs(delta)) |>
    ggplot(aes(as_factor(perm), abs_delta)) +
    geom_boxplot(aes(fill = as_factor(n_ews))) +
    facet_grid(ews~n_ews, scales = "free")

test_all <- test_all |>
    left_join(select(samples, lon, lat, n_ews, biome)) |>
    mutate(abs_delta = abs(delta), real = perm == "real")

test_all |>
#    mutate(real = perm == "real") |>
    ggplot(aes(real, abs_delta)) +
    geom_boxplot() +
    facet_wrap(~ews, scales = "free")


#### Statistical tests ####
test_all |>
    # split(~n_ews) |>
    filter(ews == "ews_ac1") |>
    pull(abs_delta) |> #shapiro.test()
    qqnorm() # not normal



test_all |>
    left_join(
        samples |>
            select(lon,lat, starts_with("value_delta")) |>
            pivot_longer(cols = starts_with("value_delta"), names_to = "ews", values_to = "measured_ews") |>
            mutate(ews = str_replace_all(ews,"value_delta", "ews"))
    ) |>
    filter(perm == "real") |>
    ggplot(aes(delta, measured_ews)) +
    geom_point() +
    facet_wrap(~ews, scales = "free")

## there are problems with AC1, does not align on the identity line: remember, delta is the ews I just calculated in this script, measured_ews is the same stat calculated in previous rounds of the analysis for all the planet. The numbers should be the same.
## J211027: problem solved, AC1 was calculated before with Pearson coef which is faster, the second time used Kendal for my testing but it should be Person to match previous results.


## not normal as expected, so no t-test
ktest <- test_all |>
    split(~ews) |>
    map(~kruskal.test(abs_delta ~ real, data = .))

wtest <- test_all |>
    split(~ews) |>
    map(~wilcox.test(abs_delta ~ real, data = .))

save(test_all, file = "Results/perm_results_chlorA_211101.RData")

## This is really useful, showing that after 3 ews there is high confidence that something
## is going on. However the test is not fair, because it compares an ews_? with randomized
## permutations, but not necessarily for pixels where the ews_? worked. So a more fair test
## is limiting the comparison for pixels where it worked.

# df_delta_detected |>
#     filter(ews_delta_ac1 == TRUE) |>
#     select(lon, lat, biome, real_delta = value_delta_ac1) |>
#     right_join(test_all) |>
#     filter(ews == "ews_ac1", real == TRUE) |>
#     ggplot(aes(abs_delta, abs(real_delta))) +
#     geom_point() +
#     geom_smooth()

## TO-DO: there is something wrong with the code because I'm not getting exaclty the same
## values of delta. The fractal dimension code is not working, returning all zeroes, and the
##  other stats while it does not through errors, they don't match with calculations done early
## this year.


test_std |>
    filter(n_ews == 0, ews == "ews_ac1") |>
    select(lon, lat, biome, value_delta_ac1 = delta)
