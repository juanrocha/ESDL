## Scrip to prepare figures and other requests from reviewers

library(tidyverse)
library(deSolve)
library(patchwork)
library(ggridges)

## need biomes for viz
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/terrestrial_biomes.RData')
load('~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/marine_biomes.RData')
#### Compare different window sizes ####
# I re-ran the analysis with window size = 4yrs. 

load("Results/220325_deltas_gpp_log_4yr-window.RData")

deltas_4yr <- deltas

load("Results/210212_deltas_gpp_log.RData")

deltas_t <- deltas ## this is the half window


p1 <- deltas_t |> 
    ggplot(aes(delta_ac1)) +
    geom_density(color = "grey50",fill = "grey50", alpha = 0.4, size = 0.1) + 
    geom_vline(xintercept = quantile(deltas_t$delta_ac1, c(0.05, 0.95)),
               color = "orange", linetype = 2, size = 0.25) +
    annotate(
        "segment", x = c(-0.025, -0.4, 0.025, 0.4), y = c(5.5, 6, 5.5, 6),
        xend = c(-0.4, -0.025, 0.4, 0.025), yend = c(5.5, 6, 5.5, 6),
        color = c("red", "blue", "red", "blue"), arrow = arrow(length = unit(2, "mm")),
        size = 0.5
    ) +
    annotate(
        "text", y = c(5, 6.5), x = 0, label = c("Resilience loss", "Resilience gain"),
        color = c("red", "blue"), size = 2
    ) +
    labs(tag = "A", x = expression(Delta[AC1]), y = "Density") +
    theme_light(base_size = 6)

p2 <- deltas_t |> 
    left_join(df_biomes) |> 
    ggplot(aes(delta_ac1, biome)) +
    geom_density_ridges(aes(color = biome, fill = biome), alpha = 0.4, size = 0.1,
                show.legend = FALSE) +
    labs(tag= "B", x = expression(Delta[AC1]), y = "Terrestrial biomes")+
    scico::scale_fill_scico_d(palette = "romaO", na.value = "grey50") +
    scico::scale_color_scico_d(palette = "romaO", na.value = "grey50") +
    theme_light(base_size = 6)

load("Results/210212_deltas_chlorA_log.RData")

p3 <- deltas |> 
    left_join(df_marine) |> 
    ggplot(aes(delta_ac1, biome)) +
    geom_density_ridges(aes(color = biome, fill = biome), alpha = 0.4, size = 0.1,
                        show.legend = FALSE) +
    labs(tag= "C", x = expression(Delta[AC1]), y = "Marine realms")+
    scico::scale_fill_scico_d( palette = "hawaii", na.value = "grey50") +
    scico::scale_color_scico_d(palette = "hawaii", na.value = "grey50") +
    theme_light(base_size = 6)



p1 + p2 + p3
ggsave(plot = p1 + p2 + p3,
    filename = "deltas_AR1.png", path = "paper/figures/", width = 6, height = 2,
    dpi = 300, bg = "white", device = "png"
)

## plot the number of signals: follow 210420_figures.R
## Figure for reviewer X

pa <- deltas_4yr |> 
    left_join(df_biomes) |> 
    ggplot(aes(delta_ac1, biome)) +
    geom_density_ridges(aes(color = biome, fill = biome), alpha = 0.4, size = 0.1,
                        show.legend = FALSE)  +
    scico::scale_fill_scico_d(palette = "romaO", na.value = "grey50") +
    scico::scale_color_scico_d(palette = "romaO", na.value = "grey50") +
    labs(tag= "A", title = "Delta distribution with 4 year window", x = expression(Delta [AC1]),
         y = "Terrestrial biomes") +
    theme_light(base_size = 6)


pb <- deltas_t |> 
    left_join(df_biomes) |> 
    ggplot(aes(delta_ac1, biome)) +
    geom_density_ridges(aes(color = biome, fill = biome), alpha = 0.4, size = 0.1,
                        show.legend = FALSE)  +
    scico::scale_fill_scico_d(palette = "romaO", na.value = "grey50") +
    scico::scale_color_scico_d(palette = "romaO", na.value = "grey50") +
    labs(tag= "B", title = "Delta distribution with half window", x = expression(Delta [AC1]),
         y = "Terrestrial biomes")+
    # subtitle = "Half window for GPP is 414 weeks ~ 8.25 years") +
    theme_light(base_size = 6)

pa/pb

ggsave(plot = pa/pb,
       filename = "deltas_AR1_windows.png", path = "paper/figures/", width = 4, height = 4,
       dpi = 300, bg = "white", device = "png"
)

### plot comparative maps
load("Results/220325_detected_gpp_log_4yr-window.RData")

pa <- df_ews |> 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = n_ews)) +
    scale_fill_viridis_c("Number of signals", option = "D") +
    guides(fill = guide_colourbar(
        title.position = "top",barwidth = 5, barheight = 0.4)) +
    labs(tag = "A", title = "Detected signals with 4yr window") +
    theme_void(base_size = 6) + theme(legend.position = "bottom")

load("Results/201022_detected_gpp_log.RData")

pb <- df_ews |> 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = n_ews)) +
    scale_fill_viridis_c("Number of signals", option = "D") +
    guides(fill = guide_colourbar(
        title.position = "top",barwidth = 5, barheight = 0.4)) +
    labs(tag = "B", title = "Detected signals with half length window") +
    theme_void(base_size = 6) + theme(legend.position = "bottom")

pa + pb

ggsave(plot = pa + pb,
       filename = "detected_windows.png", path = "paper/figures/", width = 6, height = 3,
       dpi = 300, bg = "white", device = "png"
)

#### Plot potentials ####

# a simple vegetation model
#params:
r = 
veg <- function(x) {r*x*(1/(x/k)) - hm (x/(x+hv))}

# a potential from Titus and Watson:
V_titus <- function(x, b, g){
    b * g * (x - sqrt(g / (3 * b^2))) - b^3 *(x - sqrt(g / (3*b^2)))^3 

}

V_titus(3, 0.3, 0.2) # working



model2 <- function(x,b,g){b*g * ( x - (sqrt(g/b^2) / sqrt(3))) - 3*b^3 * (x - (sqrt(g/b^2)/ sqrt(3)) )^2 + b*g*x}

model <- function(t, y, params) {
    with(as.list(c(y, params)), {
    x = y
    ## The model is the derivative of the potential (from Wolfram)
    dx <- b*g * ( x - (sqrt(g/b^2) / sqrt(3))) - 3*b^3 * (x - (sqrt(g/b^2)/ sqrt(3)) )^2 + b*g*x + a*x* rnorm(1,0,0.01)
    return(list(c(dx)))
    })
}

# tibble(x = seq(-20,20, by = 0.01)) |> 
#     ggplot(aes(x)) +
#     stat_function(fun = model2, args = list(b = 1, g = 3)) +
#     stat_function(fun = model2, args = list(b = 1, g = 1), color = "blue",
#                   linetype =2) +
#     stat_function(fun = model2, args = list(b = 3, g = 3), color = "orange", linetype =2) +
#     # stat_function(fun = V_titus, args = list(b = 3, g = 3), color = "red", linetype =2) +
#     #lims(y = c(-5,20)) +
#     theme_minimal()

## maybe nice to do this later with map (for elegance), but for now manually
params1 <- list(b = 1, g = 3, a = 2)
params2 <- list(b = 1, g = 1, a = 2)
params3 <- list(b = 3, g = 3, a = 2)
times <- seq(from =0, to =100, by = 0.005)
yini <- c(1.1)

out1 <- ode(y = yini, times = times, func = model, parms = params1,
           method = "bdf") # b = 1, g = 3, black basin

out2 <- ode(y = yini, times = times, func = model, parms = params2,
            method = "bdf") # b = 1, g = 1, blue basin

out3 <- ode(y = yini, times = times, func = model, parms = params3,
            method = "bdf") # b = 3, g = 3, orange basin

#head(out)
plot(x = out[1:1-length(times),2], y = out[2:length(times), 2])
plot(x = out1[,1], y = out1[,2], type = 'l')
cor(x = out[1:1-length(times),2], y = out[2:length(times), 2])

df_plot <- list(out1,out2,out3) |> 
    map(.f = function(out) {
        out <- out |> as_tibble() |> rename(x = `1`) |> 
            filter( time %in% seq(2,100,0.01)) |> 
            mutate(x = as.numeric(x), time = as.numeric(time)) |> 
            mutate(x_1 = tsibble::difference(x, 1) + x)
        return(out)
    }) |> 
    map2(.y = c("black", "blue", "orange"), .f = function(x,y){
        x$case <- y
        return(x)
    } ) |> 
    bind_rows()

pc <- df_plot |> 
    mutate(case2 = case_when(
    case == "black" ~ "AR == 0.981",
    case == "blue" ~ "AR == 0.991",
    case == "orange" ~ "AR == 0.949")) |> 
    mutate(case2 = as_factor(case2)) |> 
    ggplot(aes(x, x_1)) +
    geom_point(size = 0.1, aes(color = case2), show.legend=FALSE) +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~case2, scales = "free", ncol = 1, labeller = label_parsed) +
    scale_color_manual(values = c("black", "blue", "orange")) +
    labs(tag = "C", y = expression(x[t-1])) +
    theme_minimal(base_size = 6) 

pb <- df_plot |> 
    mutate(case2 = case_when(
        case == "black" ~ "beta == 1 ~~ gamma == 3",
        case == "blue" ~ "beta ==1 ~~ gamma == 0.9",
        case == "orange" ~ "beta == 3.5 ~~ gamma == 3"
    )) |>
    mutate(case2 = as_factor(case2)) |> 
    ggplot(aes(time, x)) +
    geom_line(size = 0.1, alpha = 1, aes(color = case), show.legend = FALSE) +
    facet_wrap(~case2, ncol = 1, scales = "free_y", labeller = label_parsed) +
    scale_color_manual(values = c("black", "blue", "orange")) +
    labs(tag = "B") +
    theme_minimal(base_size = 6) 

df_plot |> 
    group_by(case) |> 
    summarize(cor = cor(x, x_1, use = "complete.obs")) 


pa <- tibble(x = seq(-2,2, by = 0.01)) |> 
    ggplot(aes(x)) +
    stat_function(fun = V_titus, args = list(b = 1, g = 3)) +
    stat_function(fun = V_titus, args = list(b = 1, g = 0.9), color = "blue",
                  linetype =2) +
    stat_function(fun = V_titus, args = list(b = 3, g = 3), color = "orange", linetype =2) +
    # annotation_custom(
    #     grob = ggplotGrob(inset),
    #     xmin = 0, ymin = 5, xmax = 2, ymax = 20) +
    lims(y = c(-5,20)) + labs(y = "V", tag = "A") +
    theme_classic(base_size = 6) + theme(axis.text = element_blank()) 

pa + pb + pc + plot_layout(widths = c(2.5, 2.5, 1))

ggsave(
    filename = "ews_scheme.png", path = "paper/figures/", width = 6, height = 2,
    dpi = 300, bg = "white", device = "png"
)


x <- seq(-1,2, by = 0.01)
b = 3
g = 1
int <- expression(3 * 1 * (x - sqrt(1 / (3 * 3^2))) - 3^3 * ((x - sqrt(1 / (3*3^2)))^3))
pot <- eval(int) * -1
plot(x = x, y = pot, type = "l")


### for the equation in Titus (population model with Allee effect), the integral is (from Wolfram alpha):

A_titus <- function(x, r, b, C, A) {
    -1*(r*x^2 *(6*A*b^2*C - 4*A*b*x - 4*b*C*x + 3*x^2) / 12 * A * b^4 * C)
}

tibble(x = seq(-1,15, by = 0.1)) |>  
    ggplot(aes(x)) +
    stat_function(fun = A_titus, args = list(r=1, b = 6, C = 2.5, A = 1.5)) +
    stat_function(fun = A_titus, args = list(r=1, b = 5, C = 2.5, A = 1.5), 
                  linetype =2, color = "grey40") +
    stat_function(fun = A_titus, args = list(r=1, b = 3, C = 2.5, A = 1.5), 
              linetype =2, color = "orange") 


### For the cannonical equation

cannon <- function(g,x) {g*x - x^3} 

tibble(x = seq(-2,2, by = 0.1)) |>  
    ggplot(aes(x)) +
    stat_function(fun = cannon, args = list(g = 0.5)) +
    stat_function(fun = cannon, args = list(g = 1), 
                  linetype =2, color = "grey40") +
    stat_function(fun = cannon, args = list(g=4), 
                  linetype =2, color = "orange") 


## packages:
pkgs <- c("tidyverse", "tsibble", "tictoc", "future", "furrr", "slider", "progress", "fractaldim",
          "forecast", "urca", "raster", "here", "segmented", "rsample",  "sf", "fasterize",
          "corrgram", "tidymodels", "janitor", "broom", "GGally", "diptest", "MARSS")

knitr::write_bib(c(.packages(), pkgs), "packages.bib")
