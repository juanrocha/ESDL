## terrestrial eco-regions

library(tidyverse)
library(sf)
library(raster)

## keys for latitude and longitudes
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_gpp.RData')

## Terrestrial ecosystems data:
terr_eco <- st_read("~/Documents/Projects/DATA/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")

terr_eco["WWF_MHTNAM"] %>% dim()

r <- raster::raster(nrows = length(lat), ncol = length(lon), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))

biomes <- fasterize::fasterize(terr_eco, r, field = "WWF_MHTNUM")

biomes <- biomes %>% as.matrix()
colnames(biomes) <- lon
rownames(biomes) <- lat

# create a dataframe with biomes
df_biomes <- biomes %>%
  as_tibble(rownames = "lat") %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "lon",
    values_to = "biome_code"
  ) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
  filter(!is.na(biome_code))
# extract the key from shape file:
key_terrestrial <- terr_eco %>%
  as_tibble() %>%
  dplyr::select("WWF_MHTNAM", "WWF_MHTNUM") %>%
  unique() %>%
  rename(biome = 1, biome_code = 2)

df_biomes <- left_join(df_biomes, key_terrestrial)
df_biomes <- df_biomes %>%
  mutate(biome = as_factor(biome)) %>%
  mutate(biome = fct_reorder(biome, biome_code))

object.size(biomes) %>% format("Mb")

## save
save(df_biomes, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData")

## Visualize:
df_biomes %>% ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = biome)) +
  theme_void()

## Marine ecoregions:
mar_eco <- st_read("~/Documents/Projects/DATA/MEOW/meow_ecos.shp")
## keys for latitude and longitudes
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/keys_chlorA.RData')

r <- raster::raster(nrows = length(lat), ncol = length(lon), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))

mar_realms <- fasterize::fasterize(mar_eco, r, field = "RLM_CODE")

mar_realms <- mar_realms %>% as.matrix()
colnames(mar_realms) <- lon
rownames(mar_realms) <- lat

df_marine <- mar_realms %>%
  as_tibble(rownames = "lat") %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "lon",
    values_to = "biome_code"
  ) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
  filter(!is.na(biome_code))

# extract the key from the shape file:
key_marine <- mar_eco %>%
  as_tibble() %>%
  dplyr::select(REALM, RLM_CODE) %>%
  unique() %>%
  rename(realm = 1, biome_code = 2)

df_marine <- left_join(df_marine, key_marine)

df_marine <- df_marine %>%
  mutate(biome = as_factor(realm)) %>%
  mutate(biome = fct_reorder(biome, biome_code))

df_biomes %>% ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = biome)) +
  theme_void()


## save
save(df_marine, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData")
