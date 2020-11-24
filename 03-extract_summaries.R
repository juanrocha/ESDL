library(tidyverse)
library(tictoc)


setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/ews_halfwindow_terrestrial_ecosystem_respiration_log")

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_terrestrial_ecosystem_respiration_log.RData')

files <- list.files()

summary_all <- function(x){
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
    df <- df %>%
        group_by(lon)

    # detect problematic dates:
    prob_date <- df %>% filter(ews_fd == 0) %>% pull(time) %>% unique()

    #tic()
    df_results <- df %>%
        summarize(
            ## J200723: Add here some summary statistics of the original time series: variance and std of gpp (assumed to be 1)
            max_std = max(ews_std, na.rm = TRUE),
            min_std = min(ews_std, na.rm = TRUE),
            max_ac1 = max(ews_ac1, na.rm = TRUE),
            min_ac1 = min(ews_ac1, na.rm = TRUE),
            max_kur = max(ews_kur, na.rm = TRUE),
            min_kur = min(ews_kur, na.rm = TRUE),
            max_skw = max(ews_skw, na.rm = TRUE),
            min_skw = min(ews_skw, na.rm = TRUE)
        )
    #toc() # 0.32 sec

    ## Inserting here a small correction: the min of fd is always zero, the first data point on the rolling window. I remove that zero to get accurate summary results:
    df_results_fd <- df %>%
        filter(time != prob_date) %>%
        summarize(
            max_fd = max(ews_fd, na.rm = TRUE),
            min_fd = min(ews_fd, na.rm = TRUE)
        )

    df_results <- left_join(df_results, df_results_fd)

    return(df_results)
}

# test: 0.47 secs :)
tic()
files[1] %>% summary_all()
toc()

# all:
diffs <- list()

## I think this is the same as df_results from 01 script.
tic()
diffs <- files %>%
    map(., summary_all)
toc() # 20mins in sequential, 33 min LAI, 13min gpp-log, 42min chlorA-log, 25min lai-log

# latitudes are wrong, recover them from file names
lat <- files %>% str_remove("lat_") %>% str_remove(".csv") %>% as.numeric()
## Add corrected latitudes
tic()
diffs <- diffs %>%
    map2(., lat, function(x,y) x %>% add_column(lat = y))
toc() # 0.85 secs

## calculate the difference between max and min values:
tic()
df <- diffs %>%
    bind_rows() %>%
    group_by(lon, lat) %>%
    mutate(
        diff_std = max_std - min_std,
        diff_ac1 = max_ac1 - min_ac1,
        diff_kur = max_kur - min_kur,
        diff_skw = max_skw - min_skw,
        diff_fd = max_fd - min_fd
    ) %>%
    select(lon, lat, everything()) %>% # starts_with("diff")
    pivot_longer(cols = 3:last_col(),
        names_to = c(".value", "stat"),
        names_sep = "_"
    ) %>%
    pivot_longer(cols = c("max", "min", "diff"),
        names_to = "feature", values_to = "value")
toc() # 8 secs
lobstr::obj_size(df)
object.size(df) %>% format("Mb")

## Save results:
save(df, file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201029_summary_terrestrial_ecosystem_respiration_log.RData")

## can you pre-select the relevant links where larger differences are present?
