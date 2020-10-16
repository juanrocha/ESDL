library(tidyverse)
library(sparklyr)
library(tictoc)


## If working outside RStudio
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/")

# start spark connection
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk1.8.0_251.jdk/Contents/Home")

sc <- spark_connect(master = "local", version = "2.3")

## The lat info is lost when importing data this way.
tic()
dat <- spark_read_csv(sc, "ews_halfwindow_gpp/",
  columns = c(
    time = "character",#"date",
    lon = "numeric",
    gpp = "numeric",
    gpp_1d = "numeric",
    ews_std = "numeric",
    ews_ac1 = "numeric",
    ews_kur = "numeric",
    ews_skw = "numeric",
    ews_fd = "numeric"),
  memory = FALSE
  )
toc() ## 58 secs (same as my loop in plain R)
# 5 minutes!!! maybe better on plain R?
# 7 secs when memory = FALSE

### J200818: It's not working! it returns an object full of missing values and NaNs.


## extract summaries from raw data:
dat <- dat %>%
  mutate(time = )





spark_disconnect(sc)
spark_disconnect_all()
