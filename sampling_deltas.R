library(tidyverse)
library(tictoc)

## files of interest
fls <- list.files(path = "Results/", pattern = "deltas")

## load results from delta_ews.R script
load(paste0("Results/", fls[1]))

# dataset with biomes:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')

load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')


deltas %>% 
    select(lon, lat, starts_with("delta")) %>% 
    pivot_longer(starts_with("delta"), names_to = "stat", values_to = "value") %>% 
    ggplot(aes(value)) +
    geom_density() +
    facet_wrap(~stat, scales = "free")


deltas <- left_join(
    deltas, #df_biomes)
    df_marine)



quant <- deltas %>% 
    select(lon, lat, starts_with("delta"), biome) %>% 
    pivot_longer(starts_with("delta"), names_to = "stat", values_to = "value") %>%
#    filter(!is.na(value)) %>% 
    group_by(biome, stat) %>%
    summarise(q95 = quantile(value, prob = 0.95),
              q05 = quantile(value, prob = 0.05))

quant %>%
    pivot_longer(starts_with("q"), names_to = "quantile", values_to = "value") %>% 
    ggplot(aes(value, biome)) +
    geom_point() + 
    geom_line(aes(group = biome)) +
    facet_wrap(~stat, scales = 'free_x')

deltas %>% 
    select(lon, lat, starts_with("delta"), biome) %>% 
    pivot_longer(starts_with("delta"), names_to = "stat", values_to = "value") %>% 
    ggplot(aes(value, biome)) +
    geom_boxplot(aes(fill = biome, color = biome), 
                 size = 0.2, show.legend = FALSE, alpha = 0.3) +
    facet_wrap(~stat, scales = "free_x") +
    theme_light(base_size = 7)

## Detection
tic()
deltas %>% 
    select(lon, lat, starts_with("delta"), biome) %>% 
    pivot_longer(starts_with("delta"), names_to = "stat", values_to = "value") %>% 
    left_join(quant) %>% 
    mutate(
        detected = value > q95 | value < q05,
        
    ) %>% 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = detected)) +
    scale_fill_manual(values = c("grey50", "orange")) +
    facet_wrap(~stat) +
    theme_void()
toc()

## EWS types:
tic()
deltas %>% 
    select(lon, lat, starts_with("delta"), biome) %>% 
    pivot_longer(starts_with("delta"), names_to = "stat", values_to = "value") %>% 
    filter(stat == "delta_std" | stat == "delta_ac1") %>% 
    left_join(quant) %>% 
    mutate(
        ews_type = ifelse(value > q95, "CSD", 
                          ifelse(value < q05, "CSU", "none"))
    ) %>% 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = ews_type)) +
    scale_fill_manual(values = c("blue", "orange", "grey50")) +
    facet_wrap(~stat) +
    theme_void()
toc()

## detected pixels with delta: remember than now the upper and lower percentails can be
## asymmetrical (e.g. >0.4 could be >95%, but <0.2 could be <0.05%) because the distributions
## are not necessarily centered at zero. I hope the inclusion of such asymmetry improves
## explanatory power
tic()
df_delta_detected <- deltas %>% 
    select(lon, lat, starts_with("delta"), biome) %>% 
    pivot_longer(starts_with("delta"), names_to = "stat", values_to = "value") %>% 
    left_join(quant) %>% 
    mutate(ews = if_else(value > q95 | value < q05 , TRUE, FALSE))
toc() # 0.2 secs

## sample:
df_delta_detected %>% 
    group_by(lon, lat) %>% 
    summarize(n_ews = sum(ews)) %>% 
    ungroup() %>% 
    count(n_ews)

tic()
df_delta_detected  <- df_delta_detected %>% 
    select( -q95, -q05) %>% 
    group_by(lon,lat) %>% 
    mutate(n_ews = sum(ews, na.rm = TRUE)) %>% 
    ungroup() %>%  group_by(lon,lat,biome) %>% 
    pivot_wider(names_from = stat, values_from = c(ews, value)) %>% 
    mutate(
        ews_type = case_when(
            n_ews == 0 ~ "none",
            ews_delta_ac1 == TRUE & ews_delta_std == TRUE & value_delta_ac1 > 0 & value_delta_std > 0 ~ "csd",
            ews_delta_ac1 == TRUE & ews_delta_std == TRUE & value_delta_ac1 < 0 & value_delta_std < 0 ~ "csu",
            ews_delta_ac1 == TRUE & ews_delta_std == TRUE & value_delta_ac1 > 0 & value_delta_std < 0 ~ "amb",
            ews_delta_ac1 == TRUE & ews_delta_std == TRUE & value_delta_ac1 < 0 & value_delta_std > 0 ~ "amb")) 
toc() # 37 secs, 75 on marine data

### Summary stats on detection
df_delta_detected %>%
    mutate(detection = n_ews > 0) %>%
    ungroup() %>% group_by(biome, detection) %>%
    count() %>%
    ungroup() %>% 
    pivot_wider(names_from = detection, values_from = n, names_prefix = "det_") %>%
    group_by(biome) %>%
    summarize(prop = det_TRUE / (det_TRUE + det_FALSE))

## Result: Between 26-35% of the terrestrial biomes of the world are showing signals of resilience loss. 21-31 % marine rehalms
## The proportion is with respect to the total area otherwise it's a ratio

## Result: very few pixels correspond cleanly with csu (140) or csd (242). Over 1000s for marine
df_delta_detected %>% 
    select(ews_type, n_ews) %>% 
    ungroup() %>% 
    count(ews_type)

## This is a script for a sample that takes into account pixels per biome and detection: but the
## sample remains unbalanced. Does one need to make the groups of equal size? how does one handle
## groups with very few obs like mangroves?
df_delta_detected %>% 
    select(n_ews) %>% 
    mutate(detection = n_ews > 0) %>% 
    ungroup() %>% group_by(detection, biome) %>% 
    #slice_sample(prop = 0.2, replace = FALSE) %>% 
    count() %>% 
    print(n=34)


## save results:
save(df_delta_detected, 
     file = "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData")

fls

#### Do the sampling: ####

load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData")
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_GPP_log.RData")
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_TER_log.RData")


tic()
df_delta_detected <-  df_delta_detected %>% 
    mutate(biome = fct_explicit_na(biome, na_level = "(Missing)"))
toc() # 24sec


df_delta_detected %>% 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = n_ews))

detected <- df_delta_detected %>% 
    select(n_ews) %>% 
    filter(n_ews > 0)

pxls <- detected %>% 
    ungroup() %>% group_by(biome) %>% 
    tally() 

undetected <- df_delta_detected %>%  
    filter(n_ews == 0) %>% 
    ungroup() %>% #group_by(biome) %>% 
    split(., f = .$biome) %>% 
    map2(., .y = pxls$n, 
         .f = function(x,y) slice_sample(.data = x,  n = y, replace = FALSE)) %>% 
    bind_rows() %>% 
    select(lon, lat, biome, n_ews)


undetected %>% 
    ungroup() %>% group_by(biome) %>% 
    tally(name = "n2") %>% 
    left_join(pxls) %>% 
    mutate(balanced = n == n2)

pxl_sample <- bind_rows(detected, undetected)

write_csv(pxl_sample,
          file = "Results/sample_pixels_delta_TER.csv")












## compare with old results:
load("Results/200917_detected_gpp.RData")

comp <- df_ews %>% 
    rename(old_ews = n_ews) %>% 
    select(-biome_code, -c(kur:fd)) %>% 
    right_join(df_delta_detected) %>% 
    select(old_ews, n_ews) %>% 
    ungroup() 

comp[is.na(comp[,"old_ews"]), "old_ews"] <- 0
    
comp %>% 
    count(old_ews, n_ews) %>% 
    pivot_wider(names_from = n_ews, names_prefix = "n_", values_from = n)

cor(comp$old_ews > 0, comp$n_ews > 0) # not good

old_sample %>% 
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = n_ews)) 






#### old graphs ####
deltas %>% 
    filter(delta_fd != 0) %>% 
    ggplot(aes(delta_fd)) +
    geom_density() +
    geom_vline(aes(xintercept = q95), 
               data = quant %>% filter(stat == "delta_fd")) +
    geom_vline(aes(xintercept = q05), 
               data = quant %>% filter(stat == "delta_fd")) 
