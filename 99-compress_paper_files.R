library(fs)
library(here)
library(utils)
library(stringr)

figs <- dir_ls("paper/figures") 
mnsc <- "paper/ESDL_report.tex"
bib <- dir_ls("paper/") |> str_subset(".bib")
bib <- bib[-3]

zip(
    zipfile = "Rocha_ERL.zip",
    files = c(mnsc, bib, figs)
)
