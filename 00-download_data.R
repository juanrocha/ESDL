## Download data from BGI-max plank institute

library(RCurl)

url <- "ftp.bgc-jena.mpg.de/pub/outgoing/FluxCom/CarbonFluxes"


filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
