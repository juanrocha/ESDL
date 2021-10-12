library(tidyverse)
library(fs)
library(here)
library(slider)
library(tictoc)


sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_delta_GPP.csv",
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


load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_gpp_log.RData')
fls <- dir_ls("Results/ews_halfwindow_gpp_log/")

subsample <- subsample |> 
    arrange(lat) |> 
    mutate(file = paste0("Results/ews_halfwindow_gpp_log/lat_", as.character(lat), ".csv"))

subsample$file %in% fls


dat <- map2(.x = subsample$file, .y = subsample$lon, 
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


prm <- dat |> 
    group_by(lon,lat) |> 
    modelr::permute(n = 10, gpp_1d)

# Window decides the window size: 52, 52*4, length(time)/2
w <- floor(length(time-1)/2)
x <- dat |> filter(lon == -67.125, lat == -54.375) # dataset for testing
## design a function to extract the stats of interest: delta of std

delta <- function(x) {
    # x is a dataframe with lon, lat, time and gpp_1d
    x |> 
        group_by(lon, lat) |> 
        mutate(value = slider::slide_dbl(
            gpp_1d, sd, na.rm = TRUE, .before = w, .after = 0, .complete = TRUE)) |> 
        filter(!is.na(value)) |> 
        filter(value == max(value) | value == min(value)) %>%
        arrange(time) |> 
        summarize(
            min = min(value),
            max = max(value),
            delta = diff(value),
            abruptness = diff(time)
        ) #%>%  # the following step is necessary because there is non-unique values
        # on the fractal dimension. Many datapoints that meet the condition min-max.
        #filter(abruptness == min(abruptness)) %>%
        #unique() %>%  ungroup()
}

tic()
delta(dat)
toc() # 0.039s for one pixel, dat has 282 pxls ~ 10s in dat... each replica has 10 perms of dat

test_std <- list()
## I need a for loop because the lenght of elements is not the same (error when using map)
tic()
for (i in seq_along(prm$perm)) {
    test_std[[i]] <- delta(dat[prm$perm[[i]]$idx ,])
}
toc() #16secs


test_std |> map(function(x) {x$perm <- prm$.id; return(x)})


subsample
