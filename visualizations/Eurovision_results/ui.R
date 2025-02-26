library(shiny)
library(ggiraph)

data_UI <- read.csv("datasets/eurovision_results.csv")
countries<-sort(unique(data_UI$Country))
years<-sort(unique(data_UI$Year))
places<-sort(as.numeric(unique(data_UI$Grand.Final.Place)))

ui <- fluidPage(
  titlePanel("Eurovision Results"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countries", "Select Countries:", choices = countries,
                  selected="Poland", multiple = TRUE),
      sliderInput("years", "Select Year Range:", min = min(years), max = max(years),
                  value = c(min(years), max(years)), step = 1, sep = ""),
      sliderInput("places", "Select Place Range:", min = min(places), max = max(places),
                  value = c(min(places), max(places)), step = 1, sep = "")
    ),
    mainPanel(
      girafeOutput("eurovision_plot")
    )
  )
)