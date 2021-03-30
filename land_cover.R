library(tidync)
library(tidyverse)
library(tictoc)
library(future)
library(furrr)

## If working outside RStudio:
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/")

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

## Sampling data:
# Remember that sampling and regressions will be done separately for each response variable (GPP, TER, LAI, or ChlorA). So one needs to load as well the sampling the pixels for land use dataset separately. Coordinates come from:
sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_delta_TER.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        #biome_code = col_double(),
        biome = col_character(),
        n_ews = col_double()
    )) %>%
    select(lon, lat) # I only need the coordinates now

# You only need to read the datasets once:
# 2001 for TER and GPP, 1994 LAI, 1998 ClorA
nc_2001 <- files[3] %>%
    tidync() %>%
    activate(lccs_class)
nc_2018 <- files[length(files)] %>%
    tidync() %>%
    activate(lccs_class)

# the function reads the files for the begining and end of the time series, not all the files. The years are set manually to 2001 and 2018 since it is the range for most of the vars. For ClorA and LAI that goes back to the 1990s, the initial year needs to be changed. Then it calculates the proportion of change per pixel, and the vector changes per land cover class in number of 900m^2 pixels (30*30).
land_cover_change <- function(file1, file2, lons, lats){
    # Read files
    file_list <- list(file1, file2)
    # tic()
    df_files <- file_list %>%
        map(function(x){
            x %>%
                hyper_filter(
                    # original lat lon coords are used as centroids of a 0.25 degree pixel
                    # so I add +/- 0.125 in each direction
                    lat = dplyr::between(lat, (lats - 0.125), (lats + 0.125)),
                    lon = dplyr::between(lon, (lons - 0.125), (lons + 0.125))) %>%
                hyper_tibble() %>%
                mutate(year = lubridate::as_date(time) %>% lubridate::year(.))
        })
    # toc()
    # Calculate proportion of change per 0.25 pixel
    # tic()
    prop_change <- df_files %>%
        bind_rows() %>%
        select(lccs_class, lon, lat, year) %>%
        pivot_wider(values_from = lccs_class, names_from = year) %>%
        # 2001 for TER and GPP, 1994 LAI, 1998 ClorA
        mutate(changed = `1994` != `2018`) %>%
        summarize(prop_change = (sum(changed)/n())*100)
    # toc()
    # Calculate summary per land cover class
    # tic()
    pxl_summary <-  df_files %>%
        bind_rows() %>%
        select(lccs_class, lon, lat, year) %>%
        # 2001 for TER and GPP, 1994 LAI, 1998 ClorA
        filter(year == 1994 | year == 2018) %>%
        group_by(lccs_class, year) %>%
        summarize(pixels = n()) %>%
        ungroup() %>% group_by(lccs_class) %>%
        pivot_wider(
            id_cols = lccs_class,names_from = year, values_from = pixels,
            # adding 1 as missing value is to avoid division by zero
            values_fill = 1) %>% #colSums() # 6480 pixels of 30*30mts
            # 2001 for TER and GPP, 1994 LAI, 1998 ClorA
        mutate(pxl_change = (`2018`-`1994`))
    # toc()

    return(list(prop_change, pxl_summary))
}

### test: 0.31 sec, passing
tic()
land_cover_change(nc_2001, nc_2018, lons = -74, lats = 4)
toc()


tic()
test <- map2(head(sample$lon),  head(sample$lat),
             .f = land_cover_change,
             file1 = nc_2001, file2 = nc_2018)
toc() # 1.51 sec for 6 instances

# estimated time for computation = (0.31) * nrow(sample) / 60 / 60 = 4.59 hrs
# estimated memory = 30 / 6 * nrow(sample) = 266575KB ~ 266MB
# Manageable!!

plan(multicore, workers = 10) # do it in parallel
lcc_output <- list()

tic()
lcc_output <- future_map2(
    sample$lon, sample$lat,
    .f = land_cover_change,
    file1 = nc_2001, file2 = nc_2018)
toc() # 2257.325 sec, 38min , 54min LAI

object.size(lcc_output) %>% format("Mb") # 253 Mb

lcc_output <- transpose(lcc_output)

tic()
prop_change_df <- lcc_output[[1]] %>%
    bind_rows() %>%
    mutate(lon = sample$lon, lat = sample$lat)
toc() # 0.5sec

tic()
pxl_land_cover_change <- pmap(
    list(
        x = (lcc_output[[2]]),
        lon = (sample$lon),
        lat = (sample$lat)),
    .f = function(x, lon, lat) {
        x <- x %>%
        mutate(lon = lon, lat = lat)
    return(x)}) %>%
    bind_rows()
toc() # 346.474

# lobstr::obj_size(pxl_land_cover_change)
lobstr::obj_sizes(lcc_output, prop_change_df, pxl_land_cover_change)

length(lcc_output)
## skip errors (which are pixesl with only missing values)
## It was not done with the "safely" option, so no way to see if there is errors automatically
# is_ok <- lcc_output$error %>% map(function(x) is.null(x)) %>% unlist()

## the end file should be saved in the sample folder with the format: sampled_pixels_terrestrial_LCC_4GPP.RData and repeat procedure for each response variable.

save(
    pxl_land_cover_change, prop_change_df,
    file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_land_cover_TER.RData"
)

## Remember that the order of the elements in the list corresponds to the order in the "sample" object

prop_change_df %>%
    ggplot(aes(prop_change)) +
    geom_density()
