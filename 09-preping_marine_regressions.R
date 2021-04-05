## Marine ecosystems
library(tidyverse)
library(tictoc)
library(corrgram)
library(tidymodels)

set.seed(777)

# new analysis with deltas: deltas is a df with the delta and abruptness per statistic. 19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_chlorA_log.RData") #19Mb
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData") # 35Mb


## then the sample file used to extract the samples: this is pixels where ews were detected and pixels witout detection. The sampling is stratified with the same proportion per biome.
sample <- read_csv(
    file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sample_pixels_delta_chlorA.csv",
    col_types = cols(
        lon = col_double(),
        lat = col_double(),
        #biome_code = col_double(),
        biome = col_character(),
        n_ews = col_double()
    ))

## For marine ecosystems I only have T in Kelvin as explanatory variable
#### load temperature data: 5.6Gb
tic()
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/sampled_FFT_variables/sampled_pixels_delta_temperatureK_FFT_chlorA.RData')
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
toc() # 3.5hrs

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
toc()  # 79min

rm(out_temp)


## joint them all
deltas <- deltas %>% 
    left_join(df_delta_detected) %>% 
    right_join(df_temp)


save(deltas, file = "Results/regression_data_chlorA.RData")
