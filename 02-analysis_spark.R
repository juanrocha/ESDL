## trying to analyse pieces of the Earth Data cube as netCDF files from R
## with computations out of memory with Spark

library(tidyverse)
library(sparklyr)
library(ncdf4)
library(raster)
library(tictoc)
library(future)
library(furrr)
library(forecast)
library(urca)

## If working outside RStudio
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL")

# start spark connection
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk1.8.0_251.jdk/Contents/Home")

# sc <- spark_connect(master = "local", version = "2.3")
#
# spark_disconnect(sc)
# spark_disconnect_all()

### J200702: The netCDF file did not work, it was full of very small values. Alternatively I downloaded the data by chunks per latitude, the compressed file was ~800 Mb, smaller than the .nc file.
## Read nc file:
# file <- "cleaned_gpp.nc"
# # open connection
# nc <- nc_open(file)
# # read dims
# gpp <- list()
# gpp$lon <- ncvar_get(nc, "Lon")
# gpp$lat <- ncvar_get(nc, "Lat")
# gpp$time <- ncvar_get(nc, "Time")
#
# pixel <- ncvar_get(nc, "layer", start = c(436,355,1), count = c(1,1,-1), raw_datavals =  FALSE)
#
# space <- ncvar_get(nc, "layer", start = c(1,1,1), count = c(-1,-1,1), raw_datavals =  FALSE)
#
# ## Not working, all values are range() 9.96921e+36 9.96921e+36
# nc_close(nc)


setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_gpp")
files <- list.files()

#load(files[2])
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_gpp.RData')



unitroot <- function(x){ # x is the file name
    load(x)             # load the file, all objects are called mat
    ## make into a data frame
    df <- as_tibble(mat) %>%
        mutate(time = time) %>%
        pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
        filter(!is.na(gpp)) ## remove all time series where all values are NAs.
        # remember the timeseries was clean and filled gaps in Julia, so NA are for pixels with no obs over time.

    dat <- df %>%       # transform the df into a list by longitud
        group_by(lon) %>%
        group_split()

    ## Apply first diff test and unit root test to each pixel:
    first_diff <- dat  %>%
        map(., function(x){
            forecast::ndiffs(x$gpp)
        })
    unit_root <- dat  %>%
        map(., function(x){
            urca::ur.kpss(x$gpp, lags = "long", use.lag = 0) %>% slot("teststat")
        })
    # collect results into a dataframe:
    df_results <- tibble(
        ndiffs = unlist(first_diff),
        unit_root = unlist(unit_root),
        lon = unlist(lapply(dat, function(x) unique(x$lon)))
    )

    ifelse(
        dim(df)[1] == 0,
        return(NA),
        return(df_results)
    )
}

### test with 1 file
tic()
test1 <- unitroot(files[700])
toc()
# working
# safe_unitroot <- safely(unitroot)
### apply the fucntion to all files, catch results on a list:
plan(multicore, workers = 10) # do it in parallel

tic()
results <- files %>%
    future_map(unitroot)
toc() # ~7mins

not_ok <- results %>% map(function(x) is.logical(x))
not_ok <- unlist(not_ok)

results <- results[!not_ok] %>%
    map2(., lat[!not_ok], function(x,y){ x %>% add_column(lat = y)})

results <- results %>%
    bind_rows() %>%
    mutate(lon = as.numeric(lon))

## is there any observation where the number of diff was identify or where the test statistic is higher than the critical value of the kpss test?
results %>% filter(ndiffs > 0 | unit_root > 0.739)
## All time series passed the test.

#### Not run ####

### The example below runs the unit root test and first difference test for one file. Wrap it up into a function that can be applied in each file.
# tic()
# dat <- df %>%
#     group_by(lon) %>%
#     group_split()
# toc()


## Do they need additional differencing?
# example with kpss test with 52 or 0 week lag:
# dat[[1]] %>% pull(gpp) %>% ur.kpss(lags = "long", use.lag = 0) %>% summary()
## if p-value is below 1% critical value (1pct), the null hypothesis (the time serices is stationary) is accepted; if the value is > than 1pct, the null hypothesis is rejected and probably some differencing is needed.
## In addition any test for indentifying the number of seasonal differencing return that the data is not seasonal, so the Fourier transform successfully remove all seasonal trends. Below an example:
# dat[[1]] %>% pull(gpp) %>% nsdiffs(alpha = 0.01, maxD = 52, test = "ocsb")
#
# tic()
# first_diff <- dat  %>%
#     # head() %>%
#     map(., function(x){
#         forecast::ndiffs(x$gpp)
#     })
# toc()
#
# tic()
# unit_root <- dat  %>%
#     # head() %>%
#     map(., function(x){
#         urca::ur.kpss(x$gpp, lags = "long", use.lag = 0) %>% slot("teststat")
#     })
# toc()
#
# df_1diff <- tibble(
#     ndiffs = unlist(first_diff),
#     unit_root = unlist(unit_root),
#     lon = unlist(lapply(dat, function(x) unique(x$lon)))
# )


## is there any observation where the number of diff was identify or where the test statistic is higher than the critical value of the kpss test?
# df_1diff %>% filter(ndiffs > 0 | unit_root > 0.739)
### It does not seem the time series need differencing.
