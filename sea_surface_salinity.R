## ocean salinity: pre-processing data
library(tidyverse)
library(tictoc)
library(tidync)
library(future)
library(furrr)

## If working outside RStudio:
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/")

# list files:
fls <- fs::dir_ls("~/Documents/Projects/DATA/ESA_ECV_ocean_salinity/", recurse = TRUE) %>%
    str_subset(pattern = "nc$")

# load marine dataset to get the relevant lon and lats. You don't want to reanalyize all the oceans
load(file = "Results/regression_data_chlorA.RData")

head(fls) # correct, txt file avoided
length(fls)

# try with one file
tic()
dat <- fls[3000] %>%
    tidync() %>%
    activate(sss)

x <- dat %>%
    hyper_filter(
        lat = dplyr::between(lat, -83.5 , 83.5 ),
        lon = dplyr::between(lon, -180, 180)
    ) %>%
    hyper_tibble()
toc()

# 0.58secs, Julia does it in 0.0054secs


ggplot(x, aes(lon,lat)) +
    geom_tile(aes(fill = sss)) +
    coord_cartesian()

coords <- tibble(
    lon = x$lon,
    lat = x$lat
)


out <- list()

extract_ts <- function(x,y, files) {
    files %>% purrr::map(., function(z) {
        z %>% tidync() %>%
            activate(sss) %>%
            # weird syntax but filter needs T/F conditions
            # (see https://docs.ropensci.org/tidync/articles/netcdf-with-tidync.html#transforms-1)
            hyper_filter(lon = lon == x, lat = lat == y) %>%
            hyper_tibble()
        })
}

out <- bind_rows(out)

#test: works!
tic()
extract_ts(x = coords$lon[1], y = coords$lat[1], files = fls[1:10])
toc() #2 secs for 10 files ~ 12 days for full dataset.
# Or 12mins per pixel without doing Fourier tranfsorm

time_series <- list()

# lots and lats translation:
# first coords from the 0.25 degree grid (the ESDL resolution used)
lon_025 <- seq(from = -179.875, to = 179.875, by = 0.25)
lat_025 <- seq(from = -47.625, to = 51.625, by = 0.25)

# second, coords of the salinity dataset


# hyper_dims(dat)
# hyper_vars(dat)
# active(dat)
# hyper_transforms(dat)
