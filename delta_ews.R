library(tidyverse)
library(tictoc)
library(here)

# J210209: I'm modifying the original script to identify from the begining the
# differences between critical slowing down and speeding up. To do so, I need to 
# recover the order of max and min in the time series, making the difference 
# according to time. So positive values will be CSD, and negative CSU. Later in the
# analysis, instead of using absolute difference and 90% quantile, use the two tails
# of the distribution. Updating also relative paths for reproducibility

wd <- here() 
key_var <- "ews_halfwindow_chlorA_log"

## load the keys to the file
load('keys_chlorA_log.RData')

files <- list.files(path = paste0("Results/", key_var))

## for the function to work I still need to declare working directory
setwd(dir = paste0(wd,"/Results/", key_var))

## the function recover min and max for each early warning statistics and calculates
## the difference. But preserve time ordering, so negative values indicate decrease, 
## positive values increase, close to zero no change. It also extracts the difference
## in time (days) between min and max as a proxy of abruptness
extract_delta <- function(x){
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
            ews_skw = col_double(),
            ews_fd = col_double()
        )
    )
    #toc() # 0.8 secs
    
    # detect problematic date:
    prob_date <- df %>% filter(ews_fd == 0) %>% pull(time) %>% unique()

    #tic()
    df_delta <- df %>%
        select(-gpp, -gpp_1d) %>% 
        filter(time != prob_date) %>% 
        pivot_longer(cols = starts_with("ews"), names_to = "ews", values_to = "value") %>% 
        filter(!is.na(value)) %>% 
        group_by(lon, ews) %>% 
        filter(value == max(value) | value == min(value)) %>%
        arrange(lon, time, ews) %>% # make sure older comes first
        summarize(
            delta = diff(value),
            abruptness = diff(time)
        ) %>%  # the following step is necessary because there is non-unique values
        # on the fractal dimension. Many datapoints that meet the condition min-max.
        filter(abruptness == min(abruptness)) %>% 
        unique() %>%  ungroup()
        
    #toc() # 3 secs
    
    return(df_delta)
}

## Testing
# test: 4.2 secs 
tic()
files[100] %>% extract_delta()
toc()

# all:
deltas <- list()

## Try on sequential.
tic()
deltas <- files %>%
    map(., extract_delta)
toc() # 29mins sequential | 87mins ChlorA

# recover latitudes from file names
tic()
lat <- files %>% str_remove("lat_") %>% str_remove(".csv") %>% as.numeric()
toc()

## Add corrected latitudes
tic()
deltas <- deltas %>%
    map2(., lat, function(x,y) x %>% add_column(lat = y))
toc() # 5 secs

deltas <- deltas %>% 
    bind_rows()

tic()
deltas <- deltas %>%
    group_by(lon,lat, ews) %>% 
    filter(delta == max(delta)) %>%
    ungroup() %>%
    mutate(ews = str_remove(ews, pattern = "ews_")) %>% 
    #group_by(lon,lat) %>% 
    pivot_wider(
        id_cols = c(lon,lat),
        names_from = ews,
        values_from = c(delta, abruptness),
        names_sep = "_"
    )
toc()

setwd(wd)

save(deltas, file = "Results/210212_deltas_chlorA_log.RData")

#### some viz ####




deltas %>%
    ggplot(aes(abruptness)) +
    geom_density() +
    facet_wrap(~ews, scales = "free")

deltas %>% 
    filter(ews == "ews_std") %>% 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = delta)) +
    scale_fill_gradient2(high = "orange", mid = "grey25", low = "pink") +
    theme_void()


deltas %>%
    dplyr::select(starts_with("delta")) %>% 
    GGally::ggpairs()
