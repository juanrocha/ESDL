### Convert the RData files into csv, it will allow working from spark.
## J200720
## juan.rocha@su.se

library(tidyverse)
library(tictoc)
library(future)
library(furrr)

setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/processed_gpp")
files <- list.files()

#load(files[2])
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_gpp.RData')


read_all <- function(x){
    load(x)             # load the file, all objects are called mat
    ## make into a data frame
    df <- as_tibble(mat) %>%
        mutate(time = time) %>%
        pivot_longer(1:1440, names_to = "lon", values_to = "gpp") %>%
        filter(!is.na(gpp))
    return(df)
}

# do it in parallel
plan(multicore, workers = 10)
tic()
results <- files %>%
    future_map(read_all, .progress = TRUE)
toc() # 1min

object.size(results) %>% format("Gb") #


setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/gpp_data_csv")

tic()
walk2(results, files,
    function(x,y){
        write_csv(x, path = str_replace(y, "RData", "csv"))
    }
)
toc() # 382secs but 8Gb of data instead of 3.9
