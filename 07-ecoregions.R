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

df_biomes %>% 
  ggplot(aes(lon, lat)) +
  geom_tile(aes(fill = biome)) +
  theme_void()


## save
save(df_marine, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData")


## Terrestrial ecosystems:
## Terrestrial ecosystems data:
terr_eco <- st_read("~/Documents/Projects/DATA/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")

terr_eco["ECO_NAME"] %>% dim()

r <- raster::raster(nrows = length(lat), ncol = length(lon), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))

terreco <- fasterize::fasterize(terr_eco, r, field = "ECO_ID_U")

terreco <- terreco %>% as.matrix()
colnames(terreco) <- lon
rownames(terreco) <- lat

# create a dataframe with biomes
df_eco <- terreco %>%
  as_tibble(rownames = "lat") %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "lon",
    values_to = "ecosystem_code"
  ) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
  filter(!is.na(ecosystem_code))

# extract the key from shape file:
key_terrestrial <- terr_eco %>%
  as_tibble() %>%
  dplyr::select("ECO_NAME", "ECO_ID_U") %>%
  unique() %>%
  rename(ecosystem = 1, ecosystem_code = 2)

df_eco <- df_eco %>% left_join(key_terrestrial)


df_eco <- df_eco %>%
  mutate(ecosystem = as_factor(ecosystem)) %>%
  mutate(ecosystem = fct_reorder(ecosystem, ecosystem_code))

object.size(df_eco) %>% format("Mb")

df_eco %>% ggplot(aes(lon,lat)) +
  geom_tile(aes(fill = ecosystem), show.legend = FALSE)


## save
save(df_eco, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/tererestrial_ecosystems.RData")


### countries
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world <- world %>% 
  filter(!is.na(iso_n3)) %>% 
  mutate(iso_n3 = as.numeric(iso_n3))

r <- raster::raster(nrows = length(lat), ncol = length(lon), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))

countries <- fasterize::fasterize(world, r, field = "iso_n3")


countries <- countries %>% as.matrix()
colnames(countries) <- lon
rownames(countries) <- lat

# create a dataframe with biomes
df_countries <- countries %>%
  as_tibble(rownames = "lat") %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "lon",
    values_to = "iso_n3"
  ) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
  filter(!is.na(iso_n3))
# extract the key from shape file:
key_countries <- world %>%
  filter(!is.na(iso_n3)) %>% 
  mutate(iso_n3 = as.numeric(iso_n3)) %>% 
  as_tibble() %>%
  dplyr::select("name", "iso_n3") %>%
  unique()

df_countries <- left_join(df_countries, key_countries)
df_countries <- df_countries %>%
  mutate(name = as_factor(name)) 

object.size(df_countries) %>% format("Mb")


## save
save(df_countries, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/countries.RData")
