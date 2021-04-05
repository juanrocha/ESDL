
library(tidyverse)
library(tictoc)
library(corrgram)
library(tidymodels)

set.seed(777)

# new analysis with deltas: deltas is a df with the delta and abruptness per statistic. 19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_terrestrial_ecosystem_respiration_log.RData") #19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_TER_log.RData") # 35Mb


## then the sample file used to extract the samples: this is pixels where ews were detected and pixels witout detection. The sampling is stratified with the same proportion per biome.
sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_delta_TER.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        #biome_code = col_double(),
        biome = col_character(),
        n_ews = col_double()
    ))
## Now load explanatory variables and reduce them immediately to the pixels on the sample
#### load precipitation data: 5.6Gb
tic()
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_precipitation_FFT_TER.RData')
toc() #34s, 5.6Gb
## reduce the out object to only the pixels sampled
out_prec <- out 
rm(out)

tic()
out_prec <- out_prec %>%
    bind_rows() %>%
    janitor::clean_names() %>%
    rename(
        prec_trend = trend,
        prec_longtv = long_term_variability,
        prec_annualc = annual_cycle,
        prec_fasto = fast_oscillations
    )
toc() # 7s

tic()
df_prec <- out_prec %>%
    ## there are a few pixels where everything is missing values, get rid of them
    filter(!is.na(prec_trend)) %>% # one var will do
    rowwise() %>%
    mutate(
        rain = sum(prec_annualc, prec_fasto, prec_longtv, prec_trend, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(lon, lat) %>%
    summarise(
        prec_mean = mean(rain, na.rm = TRUE),
        prec_var = var(rain, na.rm = TRUE),
        prec_std_longterm = sd(prec_longtv, na.rm = TRUE),
        prec_std_annual_cycle = sd(prec_annualc, na.rm = TRUE),
        prec_std_fast_oscillations = sd(prec_fasto, na.rm = TRUE ),
        .groups = "keep"
    )
toc() # 52 mins!

# calculate slopes
tic()
df_prec$prec_slope <- out_prec %>% 
    filter(!is.na(prec_trend)) %>% # one var will do
    group_by(lon, lat) %>%
    group_split() %>%
    map_dbl( ~ lm(prec_trend ~ date, data = .x) %>%
                 broom::tidy() %>%
                 # this is the slope:
                 filter(term == "date") %>% pull(estimate)
    )
toc()  # 42min

## clean up memory
rm(out_prec)
gc()



#### load temperature data: 5.6Gb
tic()
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_temperature_FFT_TER.RData')
toc() #29s
## reduce the out object to only the pixels sampled
out_temp <- out
rm(out)


tic()
out_temp <- out_temp %>%
    bind_rows() %>%
    janitor::clean_names() %>%
    rename(
        temp_trend = trend,
        temp_longtv = long_term_variability,
        temp_annualc = annual_cycle,
        temp_fasto = fast_oscillations
    )
toc() # 6s


tic()
df_temp <- out_temp %>%
    rowwise() %>%
    mutate(temp = sum(temp_annualc, temp_fasto, temp_longtv, temp_trend, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(lon, lat) %>%
    summarise(
        temp_mean = mean(temp),
        temp_var = var(temp),
        temp_std_longterm = sd(temp_longtv),
        temp_std_annual_cycle = sd(temp_annualc),
        temp_std_fast_oscillations = sd(temp_fasto),
        .groups = "keep"
    )
toc() # 37min

# calculate slopes
tic()
df_temp$temp_slope <- out_temp %>% 
    group_by(lon, lat) %>%
    group_split() %>%
    map_dbl( ~ lm(temp_trend ~ date, data = .x) %>%
                 broom::tidy() %>%
                 # this is the slope:
                 filter(term == "date") %>% pull(estimate)
    )
toc()  # 53min

rm(out_temp)


#### land use data:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_land_cover_TER.RData')


deltas <- deltas %>% 
    left_join(df_delta_detected) %>% 
    right_join(df_temp) %>% 
    right_join(df_prec) %>% 
    left_join(prop_change_df) %>% 
    left_join(
        pxl_land_cover_change %>%
            select(-c(2,3)) %>%
            ungroup() %>% 
            group_by(lon, lat) %>% 
            pivot_wider(names_from = lccs_class, values_from = pxl_change, values_fill = 0,
                        names_prefix = "lcc_") 
    )

## clean up memory
rm(df_delta_detected, df_prec, df_temp, prop_change_df, pxl_land_cover_change, sample)

save(deltas, file = "Results/regression_data_TER.RData")


#### Viz



