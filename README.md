# Ecosystems are showing symptoms of resilience loss

This repository contains the code for replicating the study [Ecosystems
are showing symptoms of resilience
loss](https://doi.org/10.1088/1748-9326/ac73a8) as well as the final
results. The study used data from the [Earth System Data
Cube](https://www.earthsystemdatalab.net/index.php) and applied early
warning signals of regime shifts to variables related to primary
productivity of ecosystems globally. The exact datasets used are
described on the paper. But in short, they included gross primary
productivity, net ecosystem respiration, leaf area index, and ocean
color as response variables, while predictors included temperature,
precipitation, salinity, land use, among others. The original data is
hosted at the ESDL. This is a science-industry consortia that uses
products from the European Space Agency and offer computational
facilities to work on high dimensional cubes using `zarr`. So raw
datasets are not stored here, they are several TB, and can be accessed
from ESA or noted otherwise (links provided on the paper).

## Data

Data pre-processing was done at the ESDL computational facilities
(hosted at the time at the Max Plank Institute of Biogeochemestry in
Jena, Germany) and using mainly `Julia`. By pre-processing, I mean
dealing with missing values, computing fast Fourier transforms, and
exporting the final time series for early warnings. The early warning
signals were computed in `R` on a laptop. Processed data and
intermediate steps are not included in the repo. The main reason is
size, the full repo currently contains close to 460Gb on files. Here is
the file structure of the repo:

``` r
library(fs)
library(tidyverse)
dir_info() |> select(path, size) |> knitr::kable()
```

| path                                            |    size |
|:------------------------------------------------|--------:|
| 00-download_data.R                              |   1.03K |
| 01-load_processed_data.R                        |  14.19K |
| 02-unit_root_test.R                             |   5.22K |
| 03-extract_summaries.R                          |   3.52K |
| 04-select_outliers.R                            |    3.2K |
| 05-segmented_regression.R                       |  13.81K |
| 06-together.R                                   |   8.93K |
| 07-ecoregions.R                                 |   5.33K |
| 08-figures.R                                    |  11.37K |
| 09-preping_marine_regressions.R                 |   6.65K |
| 09-preping_regressions.R                        |   4.79K |
| 09-regressions.R                                |  18.43K |
| 10-distinguishing_ews_type.R                    |   4.09K |
| 10-marine_regressions.R                         |  10.23K |
| 10-regressions.R                                |  19.76K |
| 11-bimodality.R                                 |     609 |
| 11-delta_regressions.R                          |   3.01K |
| 11-random_forest.R                              |   5.03K |
| 12-null_models.R                                |   4.02K |
| 12-permutations.R                               |  10.31K |
| 200622_update.R                                 |   7.28K |
| 210420_figures.R                                |  48.52K |
| 99-EWS_sparkly.R                                |    1010 |
| 99-compress_paper_files.R                       |     255 |
| 99-fractal_dimension.R                          |   6.03K |
| 99-rescue.R                                     |   5.14K |
| Carpenter-DLM.R                                 |   7.92K |
| EGU_analysis.R                                  |  11.75K |
| ESDL.Rproj                                      |     251 |
| ESDL_report.bib                                 |  28.42K |
| Exercise_1.ipynb                                |  25.14M |
| LICENSE                                         |   1.04K |
| Manifest.toml                                   |  56.04K |
| Notes.Rmd                                       |     482 |
| Notes.nb.html                                   | 765.71K |
| Project.toml                                    |     503 |
| README.Rmd                                      |   8.07K |
| README.html                                     | 637.45K |
| README.md                                       |  19.35K |
| Results                                         |   2.44K |
| Rocha_ERL.zip                                   |   21.6M |
| cleaned_gpp.nc                                  |    3.2G |
| delta_ews.R                                     |   4.06K |
| differentiate_CSD-CSU.R                         |   9.75K |
| environment.yaml                                |  11.33K |
| figures                                         |     640 |
| fourier.jl                                      |     260 |
| gpp_data_csv                                    |  22.59K |
| julia_setup.R                                   |     906 |
| land_cover.R                                    |   5.99K |
| land_cover.Rmd                                  |   7.15K |
| land_cover.nb.html                              |   1.51M |
| leftovers.jl                                    |   1.09K |
| nature.csl                                      |   4.33K |
| paper                                           |   1.53K |
| processed_chlorA_log                            |  22.62K |
| processed_chlor_a                               |  22.59K |
| processed_gpp                                   |  22.62K |
| processed_gpp_log                               |  22.62K |
| processed_lai_log                               |  22.59K |
| processed_lai_log2                              |  22.56K |
| processed_leaf_area_index                       |  22.59K |
| processed_root_moisture                         |  22.62K |
| processed_terrestrial_ecosystem_respiration     |  22.59K |
| processed_terrestrial_ecosystem_respiration_log |  22.59K |
| report_ESDL.Rmd                                 |   8.57K |
| report_ESDL.log                                 |  32.67K |
| report_ESDL.pdf                                 |   2.17M |
| report_figures.R                                |   3.76K |
| revisions.R                                     |  10.65K |
| salinity_bash.Rmd                               |   2.84K |
| salinity_bash.nb.html                           |   1.08M |
| sallinity.jl                                    |   8.55K |
| sampling_deltas.R                               |   7.47K |
| sea_surface_salinity.R                          |   1.91K |
| teaching_figs.R                                 |   4.72K |

All `R` and `Julia` scripts are included. The `.gitignore` is included
so you can see what was left out. In summary, scripts absolutely needed
for replication are numbered in ascending order, with a suggested order
of execution. Scripts that starts with `99_` or not numbered were
auxiliary files (e.g. to prep something for a conference talk) or things
that I tried and did not work. Similarly at the end of some of my
scripts you will find a section `## Leftovers` where I kept things I
tried and did not work, or that work but later I found better ways to do
it. All datasets used are harmonized at 0.25 degree resolution and
weekly observations, but different time coverage depending on variable
(see paper for details).

The folders starting with `processed_` contain `.RData` files, one per
latitude slice (N=720), that was the result of the computations at the
ESDL. These folders are not included because each one takes 1.73GB:

``` r
dir_info() |> select(path) |> 
    filter(str_detect(path, "processed"), str_detect(path, "\\.R", negate = TRUE)) |> 
    knitr::kable()
```

| path                                            |
|:------------------------------------------------|
| processed_chlorA_log                            |
| processed_chlor_a                               |
| processed_gpp                                   |
| processed_gpp_log                               |
| processed_lai_log                               |
| processed_lai_log2                              |
| processed_leaf_area_index                       |
| processed_root_moisture                         |
| processed_terrestrial_ecosystem_respiration     |
| processed_terrestrial_ecosystem_respiration_log |

## Results

The main results used are in the folder `Results/`. It contains
intermediate results folders for the early warning signals computed,
that follow the pattern: `ews_window-size_variable-used_transformed`,
where window size can be half of the time series or 4yrs, variable can
be gross primary productivity, terrestrial ecosystem respiration, leaf
area index, or chlorophill A (their abbreviations); and sometimes I did
a log transformation to reduce the influence of outliers (path ending in
`_log`). Each folder contains a `csv` file per latitude slice with the
results of the early warnings calculated; each folder takes between
20-40Gb of memory depending on variable. The rest of the results are
mainly `*.RData` files. Intermediate results typically start with the
date they were computed, followed by the type of file.

``` r
dir_info("Results/") |>
    select(path, type, size) |> 
    mutate(path = str_remove(path, "Results/")) |> 
    knitr::kable()
```

| path | type | size |
|:-----------------------------------------------------|:---------|-------:|
| 200917_detected_gpp.RData | file | 456.91K |
| 200917_detected_lai.RData | file | 484.86K |
| 200917_summary_gpp.RData | file | 26.26M |
| 200917_summary_lai.RData | file | 40.07M |
| 200918_detected_chlorA.RData | file | 756.44K |
| 200918_gpp_segmented_results.RData | file | 5.05M |
| 200918_summary_chlorA.RData | file | 53.16M |
| 200921_detected_terrestrial_ecosystem_respiration.RData | file | 500.96K |
| 200921_summary_terrestrial_ecosystem_respiration.RData | file | 25.96M |
| 201022_detected_gpp_log.RData | file | 490.06K |
| 201022_summary_gpp_log.RData | file | 26.18M |
| 201023_segmented_chlorA_log.RData | file | 9.87M |
| 201023_segmented_gpp_log.RData | file | 5.04M |
| 201024_detected_chlorA_log.RData | file | 850.63K |
| 201024_detected_lai_log.RData | file | 435.51K |
| 201024_detected_terrestrial_ecosystem_respiration_log.RData | file | 507.88K |
| 201024_summary_chlorA_log.RData | file | 52.79M |
| 201027_summary_lai_log.RData | file | 37.41M |
| 201028_segmented_lai_log.RData | file | 6.16M |
| 201028_segmented_terrestrial_ecosystem_respiration_log.RData | file | 5.11M |
| 201029_summary_terrestrial_ecosystem_respiration_log.RData | file | 25.82M |
| 201208_ews_type_chlorA_log.RData | file | 402.69K |
| 201208_ews_type_gpp_log.RData | file | 205.48K |
| 201208_ews_type_lai_log.RData | file | 225.56K |
| 201208_ews_type_terrestrial_ecosystem_respiration_log.RData | file | 213.13K |
| 20918_lai_segmented_results.RData | file | 6.18M |
| 20918_lai_segmented_results_is_ok.RData | file | 114 |
| 20921_segmented_chlorA.RData | file | 9.18M |
| 20921_segmented_terrestrial_ecosystem_respiration.RData | file | 5.11M |
| 210117_processed_reg_data_GPP.RData | file | 9.85M |
| 210212_deltas_chlorA_log.RData | file | 15.86M |
| 210212_deltas_gpp_log.RData | file | 7.87M |
| 210212_deltas_lai_log.RData | file | 9.77M |
| 210212_deltas_terrestrial_ecosystem_respiration_log.RData | file | 7.77M |
| 210301_delta_detected_ChlorA_log.RData | file | 14.41M |
| 210301_delta_detected_GPP_log.RData | file | 7.26M |
| 210301_delta_detected_TER_log.RData | file | 7.17M |
| 210318_deltas_lai_log2_longdf.RData | file | 9.79M |
| 210329_processed_reg_data_GPP.RData | file | 1.56M |
| 210526_rf_TER.RData | file | 303.88M |
| 210529_rf_GPP.RData | file | 324.12M |
| 210608_rf_chlorA.RData | file | 439.23M |
| 211108_lambda_chlorA_detected.RData | file | 1.88M |
| 211108_lambda_chlorA_non-detected.RData | file | 1.84M |
| 211108_lambda_gpp_detected.RData | file | 1.6M |
| 211108_lambda_gpp_non-detected.RData | file | 1.57M |
| 211108_lambda_ter_detected.RData | file | 1.58M |
| 211108_lambda_ter_detected2.RData | file | 1.58M |
| 211108_lambda_ter_non-detected.RData | file | 1.56M |
| 220325_deltas_gpp_log_4yr-window.RData | file | 7.87M |
| 220325_detected_gpp_log_4yr-window.RData | file | 478.56K |
| 220325_summary_GPP_log_4yr-window.RData | file | 26.99M |
| archive | directory | 1.97K |
| countries.RData | file | 117.16K |
| ews_4yr-window_GPP_log | directory | 17.06K |
| ews_halfwindow_chlorA | directory | 12.53K |
| ews_halfwindow_chlorA_log | directory | 12.53K |
| ews_halfwindow_gpp | directory | 17.09K |
| ews_halfwindow_gpp_log | directory | 17.06K |
| ews_halfwindow_lai | directory | 21.25K |
| ews_halfwindow_lai_log | directory | 21.22K |
| ews_halfwindow_terrestrial_ecosystem_respiration | directory | 17.06K |
| ews_halfwindow_terrestrial_ecosystem_respiration_log | directory | 17.09K |
| fire.RData | file | 731.21K |
| marine_biomes.RData | file | 141.54K |
| perm_results_GPP_211101.RData | file | 3.99M |
| perm_results_TER_211101.RData | file | 3.99M |
| perm_results_chlorA_211101.RData | file | 3.95M |
| regression_data_GPP.RData | file | 18.91M |
| regression_data_TER.RData | file | 18.25M |
| regression_data_chlorA.RData | file | 24.39M |
| sampled_FFT_variables | directory | 992 |
| tererestrial_ecosystems.RData | file | 188.45K |
| terrestrial_biomes.RData | file | 137.56K |

For example, the file `201023_segmented_chlorA_log.RData` contains the
results of segmented regressions for Chlorophyll-A log transformed data,
computed the 23 of October 2020. Here are examples of how the files look
like. The `delta` files contain the results of the *δ* statistic both
numeric (the difference between the extremes of the early warning
statistic), and as logical, if the pixel shows delta values on the
extremes of the distribution of delta adjusted per biome.

``` r
load("Results/210301_delta_detected_GPP_log.RData")
df_delta_detected |> 
    # only selecting one ews here ac1, but there is 5, just for nice printing
    select(n_ews, ews_delta_ac1, value_delta_ac1, biome) 
```

    ## Adding missing grouping variables: `lon`, `lat`

    ## # A tibble: 210,771 × 6
    ## # Groups:   lon, lat, biome [210,771]
    ##      lon    lat n_ews ews_delta_ac1 value_delta_ac1 biome                       
    ##    <dbl>  <dbl> <int> <lgl>                   <dbl> <fct>                       
    ##  1 -91.4 -0.125     1 FALSE                 -0.0991 Deserts and Xeric Shrublands
    ##  2 -90.9 -0.125     0 FALSE                  0.132  <NA>                        
    ##  3 -90.6 -0.125     0 FALSE                 -0.137  <NA>                        
    ##  4 -80.4 -0.125     0 FALSE                 -0.0638 <NA>                        
    ##  5 -80.1 -0.125     0 FALSE                 -0.0897 Tropical and Subtropical Dr…
    ##  6 -79.6 -0.125     0 FALSE                  0.0657 Tropical and Subtropical Mo…
    ##  7 -79.1 -0.125     0 FALSE                 -0.119  Tropical and Subtropical Mo…
    ##  8 -78.9 -0.125     1 TRUE                  -0.158  Tropical and Subtropical Mo…
    ##  9 -78.6 -0.125     0 FALSE                 -0.0629 Montane Grasslands and Shru…
    ## 10 -78.4 -0.125     0 FALSE                  0.0609 Tropical and Subtropical Mo…
    ## # ℹ 210,761 more rows

Similarly, files containing `lambda` are the results of multivariate
autoregressive state-space models
([MARSS](https://nwfsc-timeseries.github.io/MARSS/)) used to calculate
the time varying leading eigenvalues (*λ*). The files with `rf` are the
results from random forests, the files with `regression` are logistic
regressions, and the files with `perm` are the results of the
permutation tests. The files with `summary` contain the max, min and
difference values needed to calculate the delta statistic for each early
warning (lag-1 autocorrelation `ac1`, standard deviation `std`, skweness
`skw`, kurtosis `kur`, and fractal dimension `fd`). There are also
auxiliary files that are not results *per se*, for example
`terrestria_biomes.RData` and `marine_biomes.RData` are biome data for
visualizations, and `fire.RData` are pre-computed fire frequencies that
were used as control parameter in the regressions.

Please note that most of these files **are not** stored in this
repository, but the scripts provided enables you to re-create them. I
have only added the `delta_detected_variable_log` files which are the
final results and all you need to recreate the figures of the paper. As
shown above, it contains the pixel latitude, longitude coordinates,
biome type, number of early warning detected, and the numeric result of
the delta for each early warning as well as a logical value stating if
it is on the extremes of the distribution.

### Unused results

There are several files of unused results under the folder
`Results/archive`. The folders with small caps `results_` were computed
but not used in the final analysis due to quality issues. These
variables include leaf area index (LAI) and root moisture. These folders
contain `csv` files of computed early warnings, one per latitute slide,
each taking 42Gb of memory. Similarly, there are multiple files that
follow the naming structure of the key resutls, but that were computed
in 2020 or 2019 with slightly different pre-processing settings, not
used in the final manuscript. The files starting with `keys_` contain
the metadata with the vectors of longitude, latitudes and time for the
original datasets. The folder `archive` is 168Gb of size and is not
included in the repo.

``` r
dir_info("Results/archive/") |> 
    select(path, size) |> 
    mutate(path = str_remove(path, "Results/archive/")) |> 
    knitr::kable()
```

| path | size |
|:---------------------------------------------------------------|-------:|
| 190728_slopes_std_GPP_Long-Term-Variability.RData | 1.65M |
| 190728_slopes_std_GPP_trend.RData | 1.62M |
| 190728_slopes_std_chlorA_long-term-trend.RData | 3.41M |
| 190728_slopes_std_chlorA_trend.RData | 3.43M |
| 190728_std_GPP_Long-Term-Variability.RData | 12.21M |
| 190728_std_GPP_trend.RData | 12.32M |
| 190728_std_chlor_A_long-term-trend.RData | 46.39M |
| 190728_std_chlor_A_trend.RData | 46.83M |
| 200428_std_GPP_fast-oscillations.RData | 11.99M |
| 200504_p_values_sd_GPP.RData | 2M |
| 200504_slopes_sd_GPP.RData | 5.46M |
| 200504_std_GPP_fast-oscillations.RData | 116.33M |
| 200618_std_GPP_fast-oscillations_gaussian05window.RData | 209.1M |
| 200619_std_GPP_fast-oscillations_gaussian10percentwindow.RData | 210.95M |
| 200619_std_GPP_fast-oscillations_gaussian50window.RData | 211.67M |
| 200620_std_GPP_fast-oscillations_gaussian50.RData | 503.13M |
| 200620_std_GPP_fast-oscillations_gaussian50percentwindow.RData | 124.18M |
| 200621_std_clorA_fast-oscillations_gaussian50percentwindow.RData | 287.33M |
| 200626_std_chlorA_removedMSC_gaussian50.RData | 504.34M |
| 200626_std_chlorA_removedMSC_gaussian50percent.RData | 291.94M |
| 200627_std_GPP_removedMSC_gaussian50percent.RData | 125.82M |
| 200628_std_GPP_Fourier_NOGaussian_FirstDiff.RData | 207.74M |
| 200628_std_GPP_Fourier_NOgaussian.RData | 206.2M |
| 200703_std_gpp_results.RData | 6.06M |
| 200714_fd_std_gpp_results.RData | 8.38M |
| 200717_std_chlorA_results.RData | 12.05M |
| 200805_results_ews_halfwindow_LAI.RData | 16.91M |
| 200805_results_ews_halfwindow_chlorA.RData | 25.77M |
| 200805_results_ews_halfwindow_gpp.RData | 12.8M |
| 200819_summary_chlorA.RData | 49.44M |
| 200819_summary_gpp.RData | 25.25M |
| 200819_summary_lai.RData | 37.14M |
| 200825_detected_gpp.RData | 305.61K |
| 200826_summary_gpp.RData | 26.26M |
| 200826_summary_lai.RData | 40.07M |
| 200829_summary_terrestrial_ecosystem_respiration.RData | 25.96M |
| 200901_detected_terrestrial_ecosystem_respiration.RData | 630.59K |
| 200904_detected_lai.RData | 357.98K |
| 200904_summary_chlorA.RData | 53.16M |
| 200904_summary_lai.RData | 40.07M |
| 201022_results_ews_halfwindow_gpp-log.RData | 13.02M |
| 201023_results_ews_halfwindow_chlorA-log.RData | 25.99M |
| 201027_results_ews_halfwindow_lai-log.RData | 18.37M |
| 201029_results_ews_halfwindow_terrestrial_ecosystem_respiration-log.RData | 12.94M |
| 220325_results_ews_4yrwindow_GPP-log.RData | 12.97M |
| keys_LAI.RData | 8.69K |
| keys_chlorA.RData | 8.2K |
| keys_chlorA_log.RData | 8.49K |
| keys_gpp.RData | 7.95K |
| keys_gpp_log.RData | 7.95K |
| keys_lai_log.RData | 8.69K |
| keys_lai_log2.RData | 8.69K |
| keys_root_moisture.RData | 7.51K |
| keys_terrestrial_ecosystem_respiration.RData | 7.94K |
| keys_terrestrial_ecosystem_respiration_log.RData | 7.94K |
| results_ews_chlorA_halfwindow | 12.53K |
| results_ews_halfwindow_lai | 21.22K |
| results_ews_halfwindow_root_moisture_NOTUSE | 10.62K |
| results_std_GPP_Fourier_NOgaussian.csv | 16.01M |
| results_tmp_lai_210316 | 21.25K |

## Acknowledgements

This work would have not been possible without the open data provided by
Copernicus Climate Change and Atmosphere Monitoring Service, NASA,
FLUXCOM initiative, and the Global fire emissions database. I would like
to thank the European Space Agency and the Max Planck Institute of
Biogeochemistry for an early adopter grant to use the Earth System Data
Lab curated data sets and computational facilities. The paper benefited
from comments from Fabian Dablander, Steven Lade, Thorsten Blenckner,
Megan Meacham, Giesela Rohr and Stephen Carpenter. Stephen Carpenter
also provided example code to compute the dynamic linear models for
lambdas. My research was supported by Formas Grant Nos. 942-2015-731,
2020-00198 and 2019-02316, the latter through the Belmont Forum.

## Archiving

While not all files are archived here, there is a complete copy of this
repository with the intermediate results files archived at Stockholm
University. To access it please contact the archivist at the [Stockholm
Resilience Centre](https://www.stockholmresilience.org) (Tobias
Andersson at the time of writing).
