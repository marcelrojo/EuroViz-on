.libPaths(c("external_libs", .libPaths()))
library(ggflags)

library(shiny)
library(ggplot2)
library(plotly)
library(ggiraph)
library(countrycode)

data <- read.csv("datasets/eurovision_results.csv")

# Convert necessary columns
data_placements <- data[!is.na(data$Grand.Final.Place), ]
data_placements$Year <- as.numeric(data_placements$Year)
data_placements$Grand.Final.Place <- as.numeric(data_placements$Grand.Final.Place)
data_placements$Country <- factor(data_placements$Country)
data_placements$Code <- tolower(countrycode(data_placements$Country, "country.name", "iso2c"))


custom_colors <- c(
  "#fff800",  
  "#ff0188",  
  "#0043fe",  
  "#aed258",  
  "#ff7815",  
  "#9b59b6",  
  "#3498db",  
  "#e74c3c",  
  "#2ecc71",  
  "#f39c12"   
)

repeated_colors=rep(custom_colors, 6)

server <- function(input, output) {
  output$eurovision_plot <- renderGirafe({
    selected_data <- subset(data_placements, Country %in% input$countries 
                            & Year >= input$years[1] & Year <= input$years[2]
                            & Grand.Final.Place >= input$places[1] & Grand.Final.Place <= input$places[2])
    
    # Plot selected data
    p <- ggplot(selected_data, aes(x = Year, y = Grand.Final.Place, group = Country, color = Country, fill=Country)) +
      geom_line(size = 1.5) +
      geom_point_interactive(aes(tooltip=paste0("Country: ", Country, "<br>",
                                                "Year: ", Year, "<br>",
                                                "Place: ", Grand.Final.Place, "<br>",
                                                "\"", Song, "\" by ", Artist)), size = 9.5, alpha=1) + # Plot all places as small circles
      geom_flag(aes(country = Code, color = Country), size = 8) +      
      scale_shape_manual(name = "", values = 16) + # Change shape to a circle
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
    widg
  })
}