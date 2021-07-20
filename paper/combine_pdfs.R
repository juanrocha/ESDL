install.packages("qpdf")
library(qpdf)

setwd("paper/")
pdf_combine(
    input = c("main_Science-template-2021.pdf", "sm_only.pdf"), 
    output = "combined_science.pdf")
