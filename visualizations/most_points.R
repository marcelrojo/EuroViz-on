#From 1956 to 1974 points distrubution was irregullar and incosnistent.
#From 1975 to 2015 1 2 3 4 5 6 7 8 10 12 point system
#From 2016 to 2024 2x 1 2 3 4 5 6 7 8 10 12 point system 
library(dplyr)
library(plotly)
library(ggplot2)
library(ggiraph)
#devtools::install_github("ellisp/ggflags")
library(ggflags)
library(countrycode)

data=read.csv("eurovision_results.csv")


#Make bar charts of top 10 songs with most points in the final in each system
#Filter if points is not numerical value

most_points_old_pre_semis <- data %>%
  filter(
    Year >= 1975 & Year <= 2003,
    !is.na(as.numeric(Grand.Final.Points))
  )%>%
  mutate(Grand.Final.Points = as.integer(Grand.Final.Points)) %>%
  arrange(desc(Grand.Final.Points)) %>%
  head(10)
most_points_old_pre_semis$Code <- tolower(countrycode(most_points_old_pre_semis$Country, "country.name", "iso2c"))

  
most_points_new <- data %>%
  filter(
    Year >= 2016 & Year <= 2024,
    !is.na(as.numeric(Grand.Final.Points))  
  ) %>%
  mutate(Grand.Final.Points = as.integer(Grand.Final.Points)) %>%
  arrange(desc(Grand.Final.Points)) %>%
  head(10)
most_points_new$Code <- tolower(countrycode(most_points_new$Country, "country.name", "iso2c"))


most_points_old <- data %>%
  filter(
    Year >= 2004 & Year <= 2015,
    !is.na(as.numeric(Grand.Final.Points)) 
  ) %>%
  mutate(Grand.Final.Points = as.integer(Grand.Final.Points)) %>%
  arrange(desc(Grand.Final.Points)) %>%
  head(10)
most_points_old$Code <- tolower(countrycode(most_points_old$Country, "country.name", "iso2c"))

custom_colors <- c(
  "#6B10C5",
  "#aa20ab",  
  "#d9009b",
  "#ff1e6e",
  "#ff5f49",  
  "#FF7814",
  "#ff9e35",  
  "#ffa65b",
  "#ffd254",
  "#f9f871"
)

#Most points received in the new system
g <- ggplot(most_points_new, aes(x = reorder(Song, Grand.Final.Points), y = Grand.Final.Points, fill = Country)) +
  geom_col_interactive(aes(tooltip = paste0(Country, "<br> \"", Song,"\" by ", Artist, "<br>Year: ", Year, "<br>Points: ", Grand.Final.Points))) +
  geom_text(aes(x = reorder(Song, Grand.Final.Points), y = max(Grand.Final.Points)/38, label = paste0("\"",Song,"\"")), 
            color = "white", size = 5, fontface = "bold", vjust = 0.5, hjust = 0) +
  geom_flag(aes(y=0, country=Code), size=10)+
  scale_fill_manual(values = custom_colors) +
  labs(title = "Most points received 2016-2024") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#010039"),
    plot.background = element_rect(fill = "#010039"),
    axis.text.y = element_blank(),  
    axis.title.y = element_blank(), 
    plot.title = element_text(color = "white", size = 20, hjust = 0.5),
    axis.text = element_text(color = "white", size = 14, face = "bold"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank() 
  )

widg <- girafe(ggobj = g, width_svg = 10, height_svg = 10)
widg

#Most points received in the old system with more countries taking part -> semi finals starte to happen
g <- ggplot(most_points_old, aes(x = reorder(Song, Grand.Final.Points), y = Grand.Final.Points, fill = Country)) +
  geom_col_interactive(aes(tooltip = paste0(Country, "<br> \"", Song,"\" by ", Artist, "<br>Year: ", Year, "<br>Points: ", Grand.Final.Points))) +
  geom_text(aes(x = reorder(Song, Grand.Final.Points), y = max(Grand.Final.Points)/38, label = paste0("\"",Song,"\"")), 
            color = "white", size = 5, fontface = "bold", vjust = 0.5, hjust = 0) +
  geom_flag(aes(y=0, country=Code), size=10)+
  scale_fill_manual(values = custom_colors) +
  labs(title = "Most points received 2004-2015") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#010039"),
    plot.background = element_rect(fill = "#010039"),
    axis.text.y = element_blank(),  
    axis.title.y = element_blank(), 
    plot.title = element_text(color = "white", size = 20, hjust = 0.5),
    axis.text = element_text(color = "white", size = 14, face = "bold"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank() 
  )

widg <- girafe(ggobj = g, width_svg = 10, height_svg = 10)
widg

#Most points received in old system before the semi finals
g <- ggplot(most_points_old_pre_semis, aes(x = reorder(Song, Grand.Final.Points), y = Grand.Final.Points, fill = Country)) +
  geom_col_interactive(aes(tooltip = paste0(Country, "<br> \"", Song,"\" by ", Artist, "<br>Year: ", Year, "<br>Points: ", Grand.Final.Points))) +
  geom_text(aes(x = reorder(Song, Grand.Final.Points), y = max(Grand.Final.Points)/38, label = paste0("\"",Song,"\"")), 
            color = "white", size = 5, fontface = "bold", vjust = 0.5, hjust = 0) +
  geom_flag(aes(y=0, country=Code), size=10)+
  scale_fill_manual(values = custom_colors) +
  labs(title = "Most points received 1975-2003") +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#010039"),
    plot.background = element_rect(fill = "#010039"),
    axis.text.y = element_blank(),  
    axis.title.y = element_blank(), 
    plot.title = element_text(color = "white", size = 20, hjust = 0.5),
    axis.text = element_text(color = "white", size = 14, face = "bold"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank() 
  )

widg <- girafe(ggobj = g, width_svg = 10, height_svg = 10)
widg

