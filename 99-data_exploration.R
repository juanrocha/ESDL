library(tidyverse)
library(plotly)

## FAO data on crops
file <- "~/Documents/Projects/DATA/FAOSTAT/CommodityBalances_Crops_E_All_Data_(Normalized).csv"

dat <- read_csv(file)

g1 <- dat %>%
  filter(Area == "Mexico") %>%
  filter(Element == "Domestic supply quantity") %>%
  ggplot(aes(Year, Value)) +
  geom_line(aes(group = Item), show.legend = FALSE) +
  labs(
    title = "Domestic supply quantity",
    subtitle = "Unit values are on tonnes as reported by countries",
    caption = "Data: FAO stats, @juanrocha") +
  theme_light()

g2 <- dat %>%
  filter(Area == "Mexico") %>%
  filter(Element == "Export Quantity") %>%
  ggplot(aes(Year, Value)) +
  geom_line(aes(group = Item), show.legend = FALSE) +
  labs(
    title = "Export Quantity",
    subtitle = "Unit values are on tonnes as reported by countries",
    caption = "Data: FAO stats, @juanrocha") +
  theme_light()

ggplotly(g2)
