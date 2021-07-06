# Distinguishing early warning types
library(tidyverse)
library(tictoc)



## load dataset with identified pixels from segmented regression script:
files <- list.files(path = "Results/") 

## filter to the latest segmented results with log
files <- files[str_detect(files, "segmented") & str_detect(files,"log")]


load(paste0("Results/", files[4]))
ls()

df_ews_type <- px_results %>%
    mutate(ews_type  = slope2 > slope1, up = slope2 > 0) %>%
    dplyr::select(lon, lat, up, stat) %>%
    pivot_wider(names_from = stat, values_from = up) %>%
    mutate(
        csd = ifelse((ac1 == TRUE & std == TRUE), TRUE, NA ),
        csu = ifelse((ac1 == FALSE & std == FALSE), TRUE, NA),
        ksk = ifelse((kur == TRUE & skw == TRUE | kur == FALSE & skw == FALSE), TRUE, NA),
        ksk_amb = ifelse(kur == TRUE & skw == FALSE | kur == FALSE & skw == TRUE, TRUE, NA),
        amb = ifelse(ac1 == TRUE & std == FALSE | ac1 == FALSE & std == TRUE, TRUE, NA)
    ) %>% #rowwise() %>%
    mutate(
        other = ifelse(is.na(csd) & is.na(csu) & is.na(ksk) & is.na(ksk_amb) & is.na(amb), TRUE, NA)
    ) 



## save
save(df_ews_type, file = "/Users/juanrocha/Documents/Projects/ESDL_earlyadopter/ESDL/Results/201208_ews_type_terrestrial_ecosystem_respiration_log.RData")


## visualization of proportions:

load("Results/201208_ews_type_chlorA_log.RData")
l <- nrow(df_ews_type)

df1 <- df_ews_type %>%
    summarize(
        csd = sum(csd, na.rm = TRUE) / l,
        csu = sum(csu, na.rm = TRUE) / l,
        ksk = sum(ksk, na.rm = TRUE) / l,
        ksk_amb = sum(ksk_amb, na.rm = TRUE) / l,
        amb = sum(amb, na.rm = TRUE) / l,
        other = sum(other, na.rm = TRUE) / l,
        var = "Chlorophyll A"
    )


load("Results/201208_ews_type_gpp_log.RData")
l <- nrow(df_ews_type)

df2 <- df_ews_type %>%
    summarize(
        csd = sum(csd, na.rm = TRUE) / l,
        csu = sum(csu, na.rm = TRUE) / l,
        ksk = sum(ksk, na.rm = TRUE) / l,
        ksk_amb = sum(ksk_amb, na.rm = TRUE) / l,
        amb = sum(amb, na.rm = TRUE) / l,
        other = sum(other, na.rm = TRUE) / l,
        var = "Gross primary productivity"
    )
load("Results/201208_ews_type_lai_log.RData")
l <- nrow(df_ews_type)

df3 <- df_ews_type %>%
    summarize(
        csd = sum(csd, na.rm = TRUE) / l,
        csu = sum(csu, na.rm = TRUE) / l,
        ksk = sum(ksk, na.rm = TRUE) / l,
        ksk_amb = sum(ksk_amb, na.rm = TRUE) / l,
        amb = sum(amb, na.rm = TRUE) / l,
        other = sum(other, na.rm = TRUE) / l,
        var = "Leaf area index"
    )
load("Results/201208_ews_type_terrestrial_ecosystem_respiration_log.RData")
l <- nrow(df_ews_type)

df4 <- df_ews_type %>%
    summarize(
        csd = sum(csd, na.rm = TRUE) / l,
        csu = sum(csu, na.rm = TRUE) / l,
        ksk = sum(ksk, na.rm = TRUE) / l,
        ksk_amb = sum(ksk_amb, na.rm = TRUE) / l,
        amb = sum(amb, na.rm = TRUE) / l,
        other = sum(other, na.rm = TRUE) / l,
        var = "Terrestrial ecosystem respiration"
    )

#### combine them

df_all <- bind_rows(df1, df2, df3, df4) %>%
    rename(Ambiguous = "amb", `Critical slowing down` = "csd", `Critical speeding up` = "csu",
           `Kurtosis-Skewness agreement` = "ksk", `Kurtosis-Skewness ambiguous` = "ksk_amb",
           Other = "other")

#J201208: Note that the categories are not mutually exclusive, a pixel can show CSD and KSK for example.

g2 <- df_all %>%
    pivot_longer(
        cols = !var, names_to = "ews_type", values_to = "proportion") %>%
    mutate(ews_type = as_factor(ews_type)) %>% 
    ggplot(aes(var, proportion)) +
    geom_col(aes(fill = fct_rev(ews_type)), position = "stack") +
    scale_fill_brewer("Early warning type", palette = "Dark2", 
                      guide = guide_legend(title.position = "top", nrow = 3, reverse = TRUE)) +
    labs(x = "Variable", y = "Proportion") +
    coord_flip() +
    theme_bw(base_size = 6) + labs(tag = "B") +
    theme(legend.position = "bottom", 
          legend.key.size = unit(0.25, "cm"),
          legend.text = element_text(size = 4), legend.title = element_text(size = 5))

ggsave(filename = "figures/ews_type.png", device = "png", dpi = 300, width = 4, height = 3)
