library(tidyverse)
library(sparklyr)
library(tictoc)


## If working outside RStudio
setwd("~/Documents/Projects/ESDL_earlyadopter/ESDL")

# start spark connection
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk1.8.0_251.jdk/Contents/Home")

sc <- spark_connect(master = "local", version = "2.3")

## The lat info is lost when importing data this way.
tic()
dat <- spark_read_csv(sc, "gpp_data_csv/",
  columns = c(time = "character", lon = "numeric", gpp = "numeric"))
toc() ## 58 secs (same as my loop in plain R)







spark_disconnect(sc)
spark_disconnect_all()
