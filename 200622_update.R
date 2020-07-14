## Figures for EGU meeting
## 200429

library(tidyverse)
#library(earlywarnings)
library(progress)
library(segmented)
library(tictoc)

### About parallel computing
### There is many ways of working in parallel, but two available options are:
### a) Working with `future`, `purrr` and `furrr` which are `purrr` friendly
### b) working with `parallel` which is the normal option with base R. It has a
### 'mclapply()' function that allow the setting of how many mc.cores = X to use
### However, it does not do well catching up errors, if one of the elements of
### the sublist fails, all the core returns empty results. I prefer `purrr` but
### my understanding is that these are packages under development.
library(future)
library(furrr)
library(forecast)
library(urca)


load('~/Documents/Projects/ESDL_earlyadopter/ESDL/200628_std_GPP_Fourier_NOgaussian.RData')

# skimr::skim(dat)
object.size(dat) %>% format("Mb")

## In previous versions, dat was a list with a dataframe per pixel.
tic()
keys <- dat %>%
    group_by(id) %>%
    group_keys()

dat <- dat %>%
    group_by(id) %>%
    group_split()
toc() # 26 secs for both!
## equivalent:
### Make dat into a list by pixel
# tic()
# dat <- dat %>%
#   split(.$id)
# toc() # 46 secs

### remove the id column, makes the object smaller in memory
tic()
dat <- dat %>%
    map(function(x){x %>% select(-id)})
toc()/60 # > 5min, dat is now ~250Mb in land, >10mins in oceans

object.size(dat) %>% format("Mb") # 904Mb for oceans

### seggy is a function that performs the segmented regression and a Davies test (if the two slopes are significantly different).

seggy <- function(x){
    df <- x
    fit <- lm(std_gpp ~ window, data = df)
    # no predefined psi (break point)
    sfit <- segmented::segmented(fit, ~ window)
    # no predefined k (number of points to evaluate hypothesis)
    davies <- segmented::davies.test(fit , ~ window, k = 10)
    return(list(sfit, davies))
}

seggy_safe <- safely(seggy)


## The marine data is too large to fit and manipulate into memory (>44million rows / obs). The strategy is to split the list into 4 chuncks of 11M each, run the analysis on each of them and then merge the results at the end.
# l <- 44390256 # number of rows in dat
length(dat)

# dat1 <- dat[1:209388]
# dat2 <- dat[209389:418776]


##################################
## working on parallel
plan(multicore, workers = 10) # only use in Mac outside RStudio
## without parallel it takes 0.271 sec for 5 elements =
(((0.331 / 5) * length(dat1))/60)/60
## 3.17 hours in sequence. With 10 cores/ workers, hopefully that goes down to 30+mins
tic()
models <- dat %>%
    # head() %>%
    future_map(seggy_safe, .progress = TRUE)
toc() ## 137min
## models is 14.1b in memory... treat it with care.
object.size(models) %>% format("Gb")

### extract the slopes:
slopes <- list()
tic()
for (i in 1:length(dat)){
    ifelse(
        class(models[[i]][[1]][[1]])[1] == "segmented", # conditional clause
        slopes[[i]]<- models[[i]]$result[[1]] %>%
            segmented::slope() %>%
            unlist() %>%
            enframe() %>%
            filter(name == "window1" | name == "window2") %>%
            pivot_wider(names_from = name, values_from = value) %>%
            rename(slope1 = window1, slope2 = window2) %>%
            mutate(diff = slope2 - slope1),                         # TRUE
        slopes[[i]] <- tibble(slope1 = NA, slope2 = NA, diff = 0)     # FALSE
    )
}
toc() # 32min # 54mins, 0.2Gb
object.size(slopes) %>% format("Gb")


## Run until here automatically: you can code here to safe
# slopes %>%
#     bind_rows() %>%
#     mutate(id = keys$id) %>%
#     separate(id, into = c('lon','lat'), sep = '_', convert = TRUE) %>%
#     ggplot(aes(lon, lat)) +
#     geom_tile(aes(fill = diff)) +
#     scale_fill_gradient2(high = 'black', low = 'dodgerblue', mid = 'grey84', na.value = "grey50") +
#     theme_void()


# p-values
tic()
p_values <- 1:length(models) %>%
    future_map(
        function(x){
            tibble(
                p_value = models[[x]][[1]][[2]]$p.value,
                break_point = models[[x]][[1]][[2]]$statistic
            )
        }
    )
toc() #<7mins

###

slopes <- slopes %>%
    bind_rows() %>%
    mutate(id = keys$id)
p_values <- p_values %>%
    bind_rows() %>%
    mutate(id = keys$id)


df_dat <- left_join(slopes, p_values) %>%
    mutate(key = row_number())
## fix coordinates
df_dat <- df_dat %>%
    separate(id, into = c('lon','lat'), sep = '_', convert = TRUE)

## To reduce impact on memory remove the models object. If you need something, you can re-run seggy in any of the dat objects, but you don't need 14.1Gb on RAM.
rm(models)
write_csv(df_dat, "~/Documents/Projects/ESDL_earlyadopter/ESDL/results_std_GPP_Fourier_NOgaussian.csv")

df_dat <- read_csv("~/Documents/Projects/ESDL_earlyadopter/ESDL/results_std_GPP_Fourier_NOgaussian.csv")

## Object is 12.9Mb
tic()
df_dat %>%
    ggplot() +
    #geom_density(aes(slope1), fill = "blue", alpha = 0.5) +
    geom_density(aes(diff), fill = "red", alpha = 0.5) +
    theme_light()
toc()


g3 <- df_dat %>%
    filter(diff < 20) %>%
    # filter(lat < -5, lat > -44, lon > -163 , lon < -55 ) %>% # australia
    # filter(lat < 75, lat > 20, lon > -165 , lon < -50 ) %>% # north america
    # filter(lat < 15, lat > -60, lon > -80 , lon < -30 ) %>% # south america
    # filter(lat < 36, lat > -40, lon > -19 , lon < 57 ) %>% # africa
    # filter(lat < 80, lat > 9, lon > -16 , lon < 170 ) %>% # Eurasia
    filter(p_value < 0.05, slope2> 0.2) %>%
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = slope2)) +
    scale_fill_gradient2(high = 'red', low = 'green', mid = 'grey24', na.value = "grey84") +
    #scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    # labs(tag = "A", title = "Chlorophile A Long term trend variation", caption = "Data source: ESDL") +
    labs(title = "Gross Primary Productivity",
        subtitle = "Difference in slopes, time rolling window (50 weeks) of standard deviations, Gaussian filter with 50 weeks window",
        caption = "Data source: ESDL, @juanrocha") +
    #labs(tag = "D", title = "Gross Primary Productivity", caption = "Data source: ESDL") +
    # labs(tag = "C", title = "Gross Primary Productivity Long term trend variation", caption = "Data source: ESDL") +
    theme_void() +
    theme(legend.position = "bottom"
        #plot.background = element_rect(fill = "gray50"),
        #panel.background = element_rect(fill = "gray50")
    )  +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))


## create map
world <- ggplot(
    map_data("world") %>%
        rename(lon = long),
    aes(x = lon, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
               size = 0.01) + # fill = "#f9f9f9",
    coord_quickmap() + theme_void()

world +
    geom_tile(
        aes(fill = diff), #slope2
        data = df_dat %>%
                filter(p_value < 0.05,  diff < 20 ) ) +
    scale_fill_viridis_c(begin = 0, end = 1, direction = 1, option = "D",  na.value = "grey50") +
    guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5)) +
    theme(legend.position = "bottom")



### pull one pixel:
df_dat %>%
    filter(p_value < 0.05, diff < 20) %>%
    arrange(desc(diff)) %>%
    ggplot(aes(slope1, slope2)) +
    geom_point(aes(color = p_value), alpha = 0.3, size = 0.5)

dat[[135461]] %>%
    ggplot(aes(window, std_gpp)) +
    geom_line()
