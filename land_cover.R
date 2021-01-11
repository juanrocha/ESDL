library(tidync)
library(tidyverse)
library(tictoc)

## Load data
## Land cover data was downloaded from ESA Copernicus facility
## Source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=overview
# The data is too large to be on the GitHub repo (~40GB)
# In a previous exploration `land_cover.Rmd` I've noticed that using only the first and last
# value on the timeseries gives already a good approximation for % of land cover change. 
# Since the regressions will be aggregated in time, no need to use the full time series.
# It reduces computation time.

files <- fs::dir_ls("~/Documents/Projects/DATA/LULCC/", recurse = 1) %>%
    str_subset(pattern = ".nc")
# where files[1] is 2002 and files[length(files)] is 2018
## variables: lccs_class, processed_flag, current_pixel_state, observation_count, change_count

# time series data frame
tic()
ts_df <- files[c(1,length(files))] %>%
    map(., function (x) {
        x %>% 
            tidync() %>%
            activate() %>%
            hyper_filter(
                # Colombia, Bogota
                lat = dplyr::between(lat, 4.625, 4.875),
                lon = dplyr::between(lon, -74.875, -74.675)) %>%
            hyper_tibble() 
    })
toc()
## some descriptive stats
skimr::skim(dat)


dat %>% ggplot(aes(lon, lat)) + 
    geom_tile(aes(fill = as.factor(lccs_class)))




