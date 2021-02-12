## Testing for bimodality
## J210119

library(tidyverse)
library(diptest)

# load detected data:
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201022_detected_gpp_log.RData')

dftest <- df_ews %>%
  arrange(desc(n_ews)) %>%
  select(lon, lat) %>%
  head()

### try with one pixel first:
path <- "~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/ews_halfwindow_gpp_log"
files <- list.files(path)

dat <- read_csv(paste0(path, "/lat_-12.375.csv"))

## recover time series for one pixel
dat %>%
  filter(lon == 39.625) %>%
  pull(gpp) %>%
  dip.test(simulate.p.value = TRUE, B = 2000) ## unimodal
