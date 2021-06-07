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

## For marine ecosystems I have T in Kelvin as explanatory variable
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

load("Results/regression_data_chlorA.RData")

## just checking
load("Results/sampled_FFT_variables/sea_surface_salinity_FFT_.RData")

# df_all %>% 
#     ggplot(aes(lon, lat)) + 
#     geom_point(color = "blue", size = 0.05)
# 
# 
# df_all %>% 
#     ggplot(aes(lon, lat)) +
#     geom_tile(aes(fill = sss_std_fast_oscillations)) +
#     scale_fill_viridis_c()

## example

lons1 <- df_all %>% pull(lon) %>% unique()
lons2 <- deltas %>% pull(lon) %>% unique()
# lons <- tibble(
#     values = c(lons1, lons2),
#     source = c(rep("sss_lon", length(lons1)), rep("ews_lon", length(lons2)))) %>% 
#     arrange(values) %>% 
#     mutate(checker = slider::slide_lgl(
#         source, function(x) x[1] == x[2], .before = 1)) %>%
#     filter(checker != TRUE) %>% 
#     select(-checker) %>% 
#     add_column(id = rep(1:(2776/2), each = 2)) %>% 
#     pivot_wider(id_cols = id, names_from = source, values_from = values) 

lats1 <- df_all %>% pull(lat) %>% unique()
lats2 <- deltas %>% pull(lat) %>% unique()
# lats <- tibble(
#     values = c(lats1, lats2),
#     source = c(rep("sss_lat", length(lats1)), rep("ews_lat", length(lats2)))) %>% 
#     arrange(values) %>% 
#     mutate(checker = slider::slide_lgl(
#         source, function(x) x[1] == x[2], .before = 1)) %>%
#     filter(checker != TRUE) %>% 
#     select(-checker) %>% 
#     add_column(id = rep(1:(780/2), each = 2)) %>% 
#     pivot_wider(id_cols = id, names_from = source, values_from = values) 

df_all %>% 
    right_join(lons, by = c("lon" = "sss_lon")) %>% 
    right_join(lats, by = c("lat" = "sss_lat"))
    
deltas


## another attempt: near() does the same with a large tolerance.
# closest <- function(x, d, l, tol) {
#     # x is the lon or lat in the undesirable resolution
#     # d is the look up table of the desirable resolution
#     # l is lon or lat
#     x0 <-  x - tol # the data is in 1/4 degree so that's the max diff
#     x1 <-  x + tol
#     z <-  d %>% 
#         dplyr::filter(x0 < eval(sym(l)), eval(sym(l)) < x1) %>% 
#         unique() %>% 
#         pull(eval(sym(l)))
#         
# 
#     if (length(z) == 1)
#         return(z)
#     else
#         s <- z[which(x-z == min(x-z))]
#     return(s)
# }

# lons1 %>% head() %>% 
#     closest(., deltas, l="lon", tol = 0.1)

# Old school for loop: approximate longituds from one projection into the closest one
# of another grid
z <- list()
diff(lons1, lag = 1) %>% range()
tol <- 0.25 # the data is in 1/4 degree so that's the max diff
for (i in seq_along(lons1)) {
    x <- lons1[i]
    x0 <-  x - tol 
    x1 <-  x + tol
    
    y1 <- lons1[lons1 < x1 & lons1 > x0]
    y2 <- lons2[lons2 < x1 & lons2 > x0]
    
    z[[i]] <- y2[which(y2-y1 == min(y2-y1))]
}

z <- unlist(z)
df_all <- df_all %>% rename(old_lon = lon, old_lat = lat)
df_temp <- tibble(
    old_lon = lons1,
    lon = z
)

df_all <- df_all %>% left_join(df_temp)

## Now with lats: a bit trickier due to unordered vector.
o <- order(lats1)
lats1 <- sort(lats1)
diff(lats1[lats1 > min(lats2) & lats1 < max(lats2) ], lag = 1) %>% range()
z <- list()
tol <- 0.1 # Lower tolerance needed
for (i in seq_along(lats1)) {
    
    x <- lats1[i]
    x0 <-  x - tol 
    x1 <-  x + tol
    
    y1 <- lats1[lats1 < x1 & lats1 > x0]
    y2 <- lats2[lats2 < x1 & lats2 > x0]
    
    ifelse(lats1[i] > max(lats2) | lats1[i] < min(lats2),
           z[[i]] <- NA,
           z[[i]] <- y2[which((y2-y1) == min((y2-y1)))]  )
    
}

z <- unlist(z)
z[o]


df_temp <- tibble(
    old_lat = lats1[o],
    lat = z[o]
)

# checks:
df_temp %>% count(lat) %>% arrange(desc(n)) #%>% print(n=100)
# expected NAs:
sum(lats1 > max(lats2) | lats1 < min(lats2))

df_all <- df_all %>% left_join(df_temp)

deltas <- deltas %>% 
    group_by(lon,lat) %>% 
    left_join(., 
        df_all %>% 
            select(-old_lon, -old_lat) %>% 
            filter(!is.na(lat), !is.na(lon)) %>% 
            group_by(lon,lat) #%>% tally() %>% arrange(desc(n)) %>% print(n=500)
        ) 

deltas %>% ungroup() %>% skimr::skim()

### joining sea-surface salinity results in many pixels with missing values


## example with GDAL 
mat <- df_coords %>% 
    filter(source == "sss") %>% 
    select(-source, lat,lon) %>% 
    as.matrix()
## I don't understand the output
res <- gdalUtils::gdaltransform(
    s_srs="EPSG:6933", t_srs="EPSG:4326", coords = mat, verbose = TRUE)



