.libPaths(c("external_libs", .libPaths()))
library(reactable)
library(ggflags)
#Dependecies issue fix
library(grImport2)
library(shiny)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(plotly)
library(ggiraph)
library(ggplot2)
library(packcircles)
library(tidyverse)
library(highcharter)
library(countrycode)
library(rsconnect)



results <- read.csv("datasets/eurovision_results.csv")
events <- read.csv("datasets/events_info.csv")


# LANGUAGES CIRCLES DATA -----------------------------------------------------
df_country_language <- data.frame(country = results$Country, language = gsub(" ", "", results$Language))
frequency_table <- table(unlist(strsplit(df_country_language$language, ",")))
prop_table <- data.frame(prop.table(frequency_table))
frequency_table <- data.frame(frequency_table)
prop_table$Count <- frequency_table$Freq
# creating a data frame with language percentage and count
language_count <- prop_table %>%
  rename(Language = Var1, Proportion = Freq)
language_count$Proportion = round(language_count$Proportion * 100, 2)
# language_count <- language_count[language_count$Proportion > 3.0, ]
# # preparing the circles plot
# packing <- circleProgressiveLayout(language_count$Proportion, sizetype = 'area')
# language_count <- cbind(language_count, packing)
# # the description
# language_count$text <-  paste("Language: ",language_count$Language,"\n",  "Percentage: ", language_count$Proportion, "%\n",
#                               "Percentage of ", language_count$Language," eurovision songs is ",language_count$Proportion,
#                               "%, that is ", language_count$Count," songs.")
# 
# # making the plot
# circle_vertices <- circleLayoutVertices(packing, npoints = 50)
# circle_vertices$id <- as.factor(circle_vertices$id)
# custom_colors_circles <- c("#6B10C5","#aa20ab","#d9009b","#ff1e6e","#ff5f49",
#                            "#FF7814","#ff9e35","#ffa65b","#ffd254","#f9f871")
# circles_viz <- ggplot() +
#   geom_polygon_interactive(data = circle_vertices, aes(x, y, group = id,
#                                                        fill = id, tooltip = language_count$text[id])) +
#   geom_text(data = language_count, aes(x, y, size = Proportion, label = Language), color = "white") +
#   scale_fill_manual(values = custom_colors_circles) +
#   scale_size_continuous(range = c(3,7)) +
#   theme_void() +
#   coord_equal() +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "#010039"),
#         plot.background = element_rect(fill = "#010039"),
#         plot.margin = unit(c(0, 0, 0, 0), 'cm'))
# 
# girafe_circles_viz <- girafe(ggobj = circles_viz,
#                              width_svg = 0.7 * 5.81,
#                              height_svg = 0.7 * 4,
#                              options = list(opts_sizing(rescale = T)))  
#griafe_circles_viz <- girafe_options(girafe_circles_viz, opts_tooltip(css = "font-family: 'Cantarell', sans-serif;"))


# COUNTRIES PLACEMENTS DATA  ---------------------------------------------------------
data_placements <- results[!is.na(results$Grand.Final.Place),]
data_placements$Year <- as.numeric(data_placements$Year)
data_placements$Grand.Final.Place <- as.numeric(data_placements$Grand.Final.Place)
data_placements$Country <- factor(data_placements$Country)
data_placements$Code <- tolower(countrycode(data_placements$Country, "country.name", "iso2c"))



function(input, output, session) { 
  # VALUE BOX ------------------------------------------------------------------
  output$contests <- renderValueBox({
    valueBox(
      value = paste(nrow(events)),
      subtitle = "ESC has been held annuualy for almost 70 years!",
      icon = icon("trophy"),
      color = "fuchsia"
    )
  })
# EUROPE MAP -----------------------------------------------------------------
# data filtered based on input from slider
  selected_data_map <- reactive({
    europe <- ne_countries(scale = "medium", returnclass = "sf", continent = c("Europe", "Asia"))
    
    year <- input$years_end
    events_filtered <- events %>% 
      filter(Year <= year)
    
    number_of_times_hosting <- events_filtered %>%
      group_by(Country) %>%
      summarise(n = n())
    europe['events'] <- number_of_times_hosting$n[match(europe$name, number_of_times_hosting$Country)] %>%
      replace_na(0) %>%
      as.factor()
    europe
  })
  
  #rendering the plot
  output$map <- renderGirafe({
    
    custom_colors_map = rev(c("#6B10C5","#a010c5", "#d9009b","#ff1e6e","#FF7814","#ffa65b","#ffd254","#f9f871","lightgrey"))
    data_map <- selected_data_map()
    map_viz <-  ggplot(data_map) +
      geom_sf_interactive(aes(fill = events, tooltip = paste("Country: ", name, "\n", 
                                                             "Number of hosted events: ", events),
                              onclick = ),
                          color = "white") +
      theme_void() +
      scale_fill_manual(values = custom_colors_map) +
      coord_sf(xlim = c(-25, 50),
               ylim = c(35, 70),
               expand = FALSE) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "#010039"),
        plot.background = element_rect(fill = "#010039"),
        plot.margin = unit(c(0, 0, 0, 0), 'cm')
      )
    # making the graph interactive 
    girafe_map_viz <- girafe(ggobj = map_viz,
                             width_svg = 2*8.9,
                             height_svg = 2*6.8436,
                             options = list(opts_sizing(rescale = T)))
    
  })
  # LANGUAGES CIRCLES ----------------------------------------------------------
  circles_data <- reactive({
    prop = input$circles_percentage

    language_count_react <- language_count[language_count$Proportion > prop, ]
    # preparing the circles plot
    
    
    packing <- circleProgressiveLayout(language_count_react$Proportion, sizetype = 'area')
    language_count_react <- cbind(language_count_react, packing)
    # the description
    language_count_react$text <-  paste("Language: ",language_count_react$Language,"\n",  "Percentage: ", language_count_react$Proportion, "%\n",
                                  "Percentage of ", language_count_react$Language," eurovision songs is ",language_count_react$Proportion,
                                  "%, that is ", language_count_react$Count," songs.")
    
    # making the plot
    #packing <- circleRepelLayout(packing, 5, 10, sizetype="area")
    
    circle_vertices <- circleLayoutVertices(packing, npoints = 50)
    circle_vertices$id <- as.factor(circle_vertices$id)
    
    
    
    custom_colors_circles <- c("#5300a6","#6B10C5", "#aa00ff","#a010c5","#aa20ab", "#d9009b", 
                              "#ff1e70","#ff1e6e","#ff5005","#ff5f49","#FF7814",
                               "#ff9e35", "#ffa65b","#febb00","#ffd254","#fffd00" ,"#f9f871")
    
    
    circles_viz <- ggplot() +
      geom_polygon_interactive(data = circle_vertices, aes(x, y, group = id,
                                                           fill = id, tooltip = language_count_react$text[id])) +
      geom_text(data = language_count_react, aes(x, y, size = Proportion, label = Language), color = "white") +
      scale_fill_manual(values = custom_colors_circles) +
      scale_size_continuous(range = c(3,7)) +
      theme_void() +
      coord_equal() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "#010039"),
            plot.margin = unit(c(-1, -1,-1, -1), 'cm'),
            panel.background = element_rect(fill = "#010039"))
    
    
    
    girafe_circles_viz <- girafe(ggobj = circles_viz,
                                 options = list(opts_sizing(rescale = T))
                                )  
    girafe_circles_viz
  })

  output$circles <- renderGirafe({
    circles_data()
  })
  # WINNERS TREEMAP ------------------------------------------------------------
  #data filtered based on the slider
  selected_data_treemap <- reactive({
    year=input$years_end
    data_finals <- results %>%
      filter(Year <= year) %>% 
      filter(Grand.Final.Place == 1) %>% 
      group_by(Country) %>%
      summarise(Count = n(),
                Wins = list(paste(Year, paste0('"', Song, '" by ', Artist), sep = " - "))) %>%
      arrange(desc(Count))
    # turn the list of winning songs to a single string for each country
    data_finals <- data_finals %>%
      mutate(Wins = sapply(Wins, paste, collapse = "<br>"))
    data_finals
  })
  #Plot treemap
  output$winners <- renderHighchart({
    custom_colors_treemap <- c("#6B10C5","#aa20ab","#d9009b","#ff1e6e"
                               ,"#ff5f49","#FF7814","#ff9e35")
    # making the treemap
    data_finals <- selected_data_treemap()
    hc <- data_finals %>% 
      hchart("treemap", hcaes(x = Country, value = Count, color = Count),
             tooltip = list(pointFormat = '<b>{point.name}</b><br>
                                          Count: {point.value}<br>
                                          Wins:<br>{point.Wins}',
                            style = list(width = '200px'))) %>%
      hc_colorAxis(
        min = 1,
        max = 7,
        stops = color_stops(
          n = 8,
          colors = custom_colors_treemap
        ),
        dataClasses = list(
          list(from = 1, to = 1, color = "#ff9e35"),
          list(from = 2, to = 2, color = "#FF7814"),
          list(from = 3, to = 3, color = "#ff5f49"),
          list(from = 4, to = 4, color = "#ff1e6e"),
          list(from = 5, to = 5, color = "#d9009b"),
          list(from = 6, to = 6, color = "#6B10C5"),
          list(from = 7, to = 7, color = "#aa20ab"),
          list(from = 8, color = "#6B10C5")
        )
      ) %>%
      hc_legend(enabled = FALSE)%>%
      hc_plotOptions(treemap = list(animation = FALSE))
    
    custom_theme <- hc_theme(chart = list(backgroundColor = "#010039"),
                             tooltip = list(backgroundColor = "#000000",
                                            style = list(color = "#ffffff",
                                                         fontFamily = "Cantarell",
                                                         fontSize = "12px")))
    # Apply the custom theme
    hc <- hc %>% hc_add_theme(custom_theme)
    hc
  })
  # POINTS SELECTOR ------------------------------------------------------------
  # data filtered based on the input from selector
  selected_data <- reactive({
    years <- strsplit(input$select, "-")
    year1 <- as.numeric(years[[1]][1])
    year2 <- as.numeric(years[[1]][2])
    most_points <- results %>% 
      filter(Year >= year1 & Year <= year2,
             !is.na(as.numeric(Grand.Final.Points))) %>%
      mutate(Grand.Final.Points = as.integer(Grand.Final.Points)) %>%
      arrange(desc(Grand.Final.Points)) %>%
      head(10)
    most_points$Code <- tolower(countrycode(most_points$Country, "country.name", "iso2c"))
    most_points
  })
  # rendering the plot 
  output$points <- renderGirafe({
    most_points <- selected_data()
    custom_colors <- c("#6B10C5","#aa20ab","#d9009b","#ff1e6e","#ff5f49",
                       "#FF7814","#ff9e35","#ffa65b","#ffd254","#f9f871")
    points_viz <- ggplot(most_points, aes(x = reorder(Song, Grand.Final.Points), y = Grand.Final.Points, 
                                          fill = reorder(Song, - Grand.Final.Points))) +
      geom_col_interactive(aes(tooltip = paste0(Country, "<br> \"", Song,"\" by ", Artist, 
                                                "<br>Year: ", Year, "<br>Points: ", Grand.Final.Points))) +
      geom_text(aes(x = reorder(Song, Grand.Final.Points), y = max(Grand.Final.Points)/38, label = paste0("\"",Song,"\"")),
                color = "white", size = 5, fontface = "bold", vjust = 0.5, hjust = 0) +
      scale_fill_manual(values = custom_colors) +
      geom_point(size=12, color="black", y=0) +
      geom_flag(aes(y=0, country=Code), size=10) +
      coord_flip() +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "#010039"),
            plot.background = element_rect(fill = "#010039"),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(color = "white", size = 20, hjust = 0.5),
            axis.text = element_text(color = "white", size = 14, face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor = element_blank())
    
    widg <- girafe(ggobj = points_viz, width_svg = 10, height_svg = 10)
  })
  
  # TIME PERIOD DESCRIPTION BOX -----------------------------------------------
  voting_format_description <- reactive({
    years <- strsplit(input$select, "-")
    year1 <- as.numeric(years[[1]][1])
    year2 <- as.numeric(years[[1]][2])
    if (year1 == "1975"){
      description <- "Between 1975 and 2003, the number of participating countries varied from 18 to 26. 
                      In most of these years, the points awarded were based either solely on televotes or solely on jury votes. 
                      Consequently, each country assigned points to their top 10 performances as follows:, 
                      1st place: 12 points, 
                      2nd place: 10 points, 
                      3rd to 10th place: 8 to 1 point respectively."
    } else if (year1 == "2004") {
      description <- "Between 2004 and 2015, the number of participating countries ranged from 36 to 43. 
                      The points awarded were based either solely on televotes or on a mixed system of jury votes and televotes, such as the 50/50 jury-televote system. 
                      Consequently, each country assigned points to their top 10 performances as follows:
                      1st place: 12 points,
                      2nd place: 10 points, 
                      3rd to 10th place: 8 to 1 point respectively."
    } else {
      description <- paste("Between 2016 and 2024, the number of participating countries ranged from 37 to 43. 
                           The points awarded were a combination of jury points and televotes. 
                           Consequently, each country assigned points to their top 10 performances as follows: 
                           1st place: 12 points (from both jury and televote), 2nd place: 10 points (from both jury and televote), 
                           3rd to 10th place: 8 to 1 point (from both jury and televote). For example, if Poland received 7 points (4th place) 
                           from the Portuguese jury and 10 points (2nd place) from the Portuguese televote, 
                           Poland would be awarded a total of 17 points from Portugal.")
    }
    description
  })
  output$description <- renderText({
    voting_format_description()
  })
  
  # COUNTRIES PLACEMENTS  ---------------------------------------------------------
  placements <- reactive({
    selected_data <- subset(data_placements, Country %in% input$countries 
                            & Year >= input$years[1] & Year <= input$years[2]
                            & Grand.Final.Place >= input$places[1] & Grand.Final.Place <= input$places[2])    
    selected_data
  })
  
  output$placements_plot <- renderGirafe({
    
    repeated_colors <- rep(c("#fff800","#ff0188","#0043fe","#aed258","#ff7815", 
                             "#9b59b6","#3498db","#e74c3c","#2ecc71","#f39c12"), 6)
    #repeated_colors=rep(custom_colors_placements, 6)
    selected_data <- placements()
    p <- ggplot(selected_data, aes(x = Year, y = Grand.Final.Place, group = Country, color = Country, fill=Country)) +
      geom_line(size = 1.5) +
      geom_point_interactive(aes(tooltip=paste0("Country: ", Country, "<br>",
                                                "Year: ", Year, "<br>",
                                                "Place: ", Grand.Final.Place, "<br>",
                                                "\"", Song, "\" by ", Artist)), size = 9.5, alpha=1) + # Plot all places as small circles
      geom_flag(aes(country = Code, color = Country), size = 8) +      
      scale_shape_manual(name = "", values = 16) + 
      scale_fill_manual(values = repeated_colors) +
      scale_color_manual(values = repeated_colors) +
      scale_y_reverse(name = "Place", breaks = seq(1, max(selected_data$Grand.Final.Place, na.rm = TRUE), 1)) +
      labs(x = "Year",
           y = "Place") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "#010039"),
            plot.background = element_rect(fill = "#010039"),
            plot.title = element_text(color = "white", size = 20, hjust = 0.5),
            axis.text = element_text(color = "white", size = 14, face = "bold"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
    
    widg <- girafe(ggobj = p, width_svg = 10, height_svg = 10)
  })
  # COUNTRIES PARTICIPATION DETAILES DATATABLE
  countries_data <- reactive({
    europe <- c(
      "Belgium", "France", "Germany", "Italy", "Luxembourg", "Netherlands", "Switzerland", 
      "Austria", "Denmark", "United Kingdom", "Sweden", "Monaco", "Norway", "Finland", 
      "Spain", "Yugoslavia", "Portugal", "Ireland", "Malta", "Greece", "Cyprus", "Iceland", 
      "Bosnia and Herzegovina", "Croatia", "Slovenia", "Estonia", "Hungary", "Lithuania", 
      "Poland", "Romania", "Russia", "Slovakia", "North Macedonia", "Latvia", "Ukraine", 
      "Albania", "Andorra", "Belarus", "Serbia and Montenegro", "Bulgaria", "Moldova", 
      "Armenia", "Czech Republic", "Georgia", "Montenegro", "Serbia", "San Marino"
    )
    asia <- c(
      "Turkey", "Israel", "Armenia", "Azerbaijan", "Georgia"
    )
    africa <- c(
      "Morocco"
    )
    oceania <- c(
      "Australia"
    )
    countries <- c()
    if ("europe" %in% input$continent) {
      countries <- c(countries, europe)
    }
    if ("asia" %in% input$continent) {
      countries <- c(countries, asia)
    }
    if ("africa" %in% input$continent) {
      countries <- c(countries, africa)
    }
    if ("oceania" %in% input$continent) {
      countries <- c(countries, oceania)
    }
    # data frame: country, first attended eurovision, last attended eurovision
    df_country_first_last = data.frame(Country = unique(results$Country))
    df_country_first_last <- df_country_first_last %>%
      filter(Country %in% countries)
    # first attended eurovision year
    min_years <- results %>%
      filter(Country %in% countries) %>%
      group_by(Country) %>%
      summarise(first_attended = min(Year, na.rm = TRUE)) 
    #last attended eurovision year
    max_years <- results %>%
      filter(Country %in% countries) %>%
      group_by(Country) %>%
      summarise(last_attended = max(Year, na.rm = TRUE))
    # how many times a country has attended eurovision
    times_attended <- results %>%
      filter(Country %in% countries) %>%
      group_by(Country) %>%
      summarise(times_attended = n_distinct(Year))
    # nr of times being in the top 5
    freq_top_5 <- results %>%
      filter(Country %in% countries) %>%
      filter(suppressWarnings(as.integer(Grand.Final.Place)) <= 5) %>%
      group_by(Country) %>%
      summarise(freq_top_5 = n())
    # best scored place
    best_scored_place <- results %>%
      filter(Country %in% countries) %>%
      mutate(Grand.Final.Place = ifelse(Grand.Final.Place %in% c("N/A", "DQ", "NQ"), "NULL", Grand.Final.Place))%>%
      group_by(Country) %>% 
      summarise(best_scored_place = as.integer(min(suppressWarnings(as.integer(Grand.Final.Place)), na.rm = TRUE)))

    
    df_country_first_last <- df_country_first_last %>%
      left_join(min_years, by = "Country") %>%
      left_join(max_years, by = "Country") %>%
      left_join(times_attended, by = "Country") %>%
      left_join(freq_top_5, by = "Country") %>%
      left_join(best_scored_place, by = "Country") %>%
      mutate(freq_top_5 = ifelse(is.na(freq_top_5), 0, freq_top_5),
             best_scored_place = ifelse(is.infinite(best_scored_place), "N/A", best_scored_place))
    names(df_country_first_last) = c("Country", "First Eurovision", "Last Eurovision", 
                                     "Times Attended", "Top 5 Placements", "Best Place Scored")
    df_country_first_last
  })
  
  output$countries_table <- renderReactable({
    countries_table_data <- countries_data()
    reactable(countries_table_data,
              theme = reactableTheme(backgroundColor = "#010039"))
  })
  
}
