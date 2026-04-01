install.packages("qpdf")
library(qpdf)

setwd("~/Downloads")
pdf_combine(
    input = c("Biosphere integrity for people and planet.pdf", "SI_WG2_Mohamed et al..pdf"),
    output = "Mohamed_Biosphere_integrity.pdf")
