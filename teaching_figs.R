#### load tools ####
library(tidyverse)
library(tictoc)
library(rworldmap)
library(sf)
library(patchwork)
library(tidymodels)
library(scico)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggnewscale)
# library(cleangeo)
data("coastsCoarse")

# load results:
# load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201029_summary_terrestrial_ecosystem_respiration_log.RData')

## newer results:
# new analysis with deltas: deltas is a df with the delta and abruptness per statistic. 19Mb

### GPP
load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/210212_deltas_gpp_log.RData") #19Mb
load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_GPP_log.RData")

### TER
# load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210212_deltas_terrestrial_ecosystem_respiration_log.RData") #19Mb
# load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_TER_log.RData")


### ClorA
load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/210212_deltas_chlorA_log.RData") #19Mb
load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData")

# dataset with biomes, ecosystems and countries:
load('~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')
load('~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')

load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/tererestrial_ecosystems.RData")
load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/countries.RData")

ls()

rsdb <- read_csv("~/Downloads/RSDB_new_format_merged_tmp.csv") |> 
    janitor::clean_names()

coord_rs <- read.csv2(
    file = '~/Documents/Projects_old/Cascading Effects/data/case_coords.csv', dec = ".") |> 
    as_tibble()

rsdb <- rsdb |> left_join(coord_rs, by = c("case_study_name" = "place")) |> 
    mutate(
        longitude = case_when(
            is.na(longitude) ~ lon,
            TRUE ~ longitude
        ),
        latitude = case_when(
            is.na(latitude) ~ lat, 
            TRUE ~ latitude
        )
    ) |> select(-lon, -lat)

trees <- read_csv("~/Downloads/GTM_full_database_download_20240509-052243.csv")

## mapping layer:
# continents <- coastsCoarse %>%
#   st_as_sf()
# ggplot() + geom_sf(data = continents) + coord_sf()
# coastsCoarse %>% ggplot() + geom_path(aes(long,lat, group = group))


## combine the two data frames:
dat <- df_delta_detected %>%
    left_join(deltas %>% select(-starts_with("delta_")))

dat <- dat %>%
    ungroup() |> 
    mutate(detected = (ews_delta_ac1|ews_delta_std|ews_delta_skw|ews_delta_kur|ews_delta_fd)) %>%
    mutate(biome2 = fct_explicit_na(biome))

rm(deltas, df_delta_detected)

## marine: load the marine datasets
dat2 <- df_delta_detected %>%
    left_join(deltas %>% select(-starts_with("delta_")))

dat2 <- dat2 %>%
    ungroup() |> 
    mutate(detected = (ews_delta_ac1|ews_delta_std|ews_delta_skw|ews_delta_kur|ews_delta_fd)) %>%
    mutate(biome2 = fct_explicit_na(biome))

rm(deltas, df_delta_detected)

dat |> 
    # filter(n_ews > 0) |> 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = n_ews, alpha = n_ews > 0))+
    scale_fill_viridis_c(
        "Number of early warnings in\n terrestrial ecosystems", option = "H",
        guide = guide_colourbar(
            title.position = "top",barwidth = 5, barheight = 0.3, order = 2) ) +
    scale_alpha_discrete(range = c(0.1,1),
        guide = guide_legend(title = "Detected early warning", title.position = "top")) +
    ggnewscale::new_scale_fill() +
    geom_tile(aes(fill = n_ews, alpha = n_ews > 0), data = dat2 ) +
    scale_fill_viridis_c(
        "Number of early warnings in\n marine ecosystems",option = "D",
        guide = guide_colourbar(
            title.position = "top",barwidth = 5, barheight = 0.3, order = 2)) +
    geom_path(
        data = map_data("world") %>% rename(lon = long),
        aes(map_id = region, group = group), size = 0.15 ) +
    geom_point(
        data = rsdb |> rename(lon = longitude, lat = latitude) |> 
            bind_rows(trees |> select(lon = long, lat)) ,
               aes(lon, lat), color = "orange", alpha = 0.5, size = 0.15) +
    theme_void(base_size = 6) + 
    ylim(c(-60,85)) +
    theme(
        legend.position = "bottom", #c(0.7,0.1),
        legend.direction = "horizontal",
        legend.box = "horizontal", legend.title.align = 0.5,
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.key.size = unit(2,"mm")
    )

ggsave(
    plot = last_plot(), filename = "rs_map.png", path = 'figures/',
    device = 'png', width = 7, height = 4, dpi = 500, bg = "white"
)


continents <- coastsCoarse %>%
  st_as_sf()
ggplot() + geom_sf(data = continents) + coord_sf()
coastsCoarse %>% ggplot() + geom_path(aes(long,lat, group = group))
