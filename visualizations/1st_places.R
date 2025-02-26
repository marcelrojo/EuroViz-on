library(dplyr)
library(tidyverse)
library(highcharter)

data <- read.csv("../eurovision_stats/datasets/eurovision_results.csv")


data_finals <- data %>%
  filter(Grand.Final.Place == 1) %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Wins = list(paste(Year, paste0('"', Song, '" by ', Artist), sep = " - "))
  ) %>%
  arrange(desc(Count))

# Combine the wins into a single string
data_finals <- data_finals %>%
  mutate(Wins = sapply(Wins, paste, collapse = "<br>"))

custom_colors <- c(
  "#6B10C5",
  "#aa20ab",  
  "#d9009b",
  "#ff1e6e",
  "#ff5f49",  
  "#FF7814",
  "#ff9e35"
)

# Create the treemap chart with customized tooltip
hc <- data_finals %>%
  hchart(
    "treemap",
    hcaes(
      x = Country,
      value = Count,
      color=Count
    ),
    tooltip = list(
      pointFormat = '<b>{point.name}</b><br>
                     Count: {point.value}<br>
                     Wins:<br>{point.Wins}',
      style = list(
        width = '200px')
    )
  ) %>% 
  hc_colorAxis(stops = color_stops(colors = rev(custom_colors)))



# Customize the tooltip appearance
hc <- hc %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = '',
    pointFormat = '<b>{point.name}</b><br>
                   Count: {point.value}<br>
                   Wins:<br>{point.Wins}'
  ) %>%
  hc_title(
    text = "Eurovision Winning Countries and Their Songs"
  )

custom_theme <- hc_theme(
  chart = list(
    backgroundColor = "#010039"
  ),
  title = list(
    style = list(
      color = "white",
      fontFamily = "Arial",
      fontSize = "20px"
    )
  ),
  tooltip = list(
    backgroundColor = "#000000",
    style = list(
      color = "#ffffff",
      fontFamily = "Verdana",
      fontSize = "12px"
    )
  )
)
# Apply the custom theme
hc <- hc %>%
  hc_add_theme(custom_theme)

hc
