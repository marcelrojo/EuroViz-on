# data
results = read.csv("eurovision_results.csv")
events = read.csv("events_info.csv")
#libraries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(plotly)
library(ggiraph)

# europe map - data
world <- ne_countries(scale = "medium", returnclass = "sf")

# dataframe country, frequency of hosting eurovsion event
hosting <- events %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# add column hosting to world, by Country and sovereignity
world['events'] <- hosting$n[match(world$name, hosting$Country)]
data.frame(world$name, world$events)


world$events[is.na(world$events)] <- 0
world$events <- as.factor(world$events)

Europe <- world[which(world$continent == "Europe"), ]
custom_colors = rev(
  c(
    "#6B10C5",
    "#d9009b",
    "#ff1e6e",
    "#FF7814",
    "#ffa65b",
    "#ffd254",
    "#f9f871",
    "lightgrey"
  )
)
p <- ggplot(world) +
  geom_sf_interactive(aes(fill = events, tooltip = paste('Country: ', name, '\n',
                                                         'Number of events hosted: ', events)),
          color = 'white',
          size = 0.1) +
  theme_void() +
  scale_fill_manual(values = custom_colors) +
  coord_sf(xlim = c(-25, 50),
           ylim = c(35, 70),
           expand = FALSE) +
  labs(fill = "Number of events") +
  theme(legend.position = "null")

widg <- girafe(ggobj = p,
               width_svg = 10,
               height_svg = 7)

widg

