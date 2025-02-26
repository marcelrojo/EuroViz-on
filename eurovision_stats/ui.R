library(shiny)
library(shinyWidgets)
library(reactable)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(plotly)
library(ggiraph)
library(shinydashboard)
library(highcharter)

data_UI<-read.csv("datasets/eurovision_results.csv")
Countries_list<-sort(unique(data_UI$Country))
Years_list<-data_UI$Year
Places_list<-sort(as.numeric(unique(data_UI$Grand.Final.Place)))

dashboardPage(
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: baseline; justify-content: center; margin-top: 2px;",
      div("Euro", style = "color: #FFF400; font-size: 18px; font-weight: 700; font-family: 'Cantarell', sans-serif;"),
      div("Viz'on", style = "color: white; font-size: 18px; font-weight: 400; font-family: 'Cantarell', sans-serif;")
    )
  ),
  dashboardSidebar(
    includeCSS("www/style.css"),
    sidebarMenu(
      id = "tabs",  
      selected = "main",  
      menuItem("Main Page", tabName = "main"),
      menuItem("Most Points Received", tabName = "most_points"),
      menuItem("Country Placements", tabName = "placements"),
      menuItem("Country Participation Details", tabName = "participation"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "main",
      includeCSS("www/style.css"),
      fluidRow(column(
        width = 10,
        div(
          style = "display: flex; align-items: baseline; justify-content: left; margin-top: 1px;",
          div("EUROVISION", style = "color: #FFF400; font-size: 30px; font-weight: 700; font-family: 'Cantarell', sans-serif; margin-right: 10px;"),
          div("STATISTICS", style = "color: white; font-size: 30px; font-weight: 400; font-family: 'Cantarell', sans-serif;")
        )
      )),
      fluidRow(
        column(
          width=12,
          tags$img(src = "Eurovision_generic_white.png", height = "100px", style = "display: block; margin: 0 auto; margin-bottom: 10px")
        ),
        column(
          width=4,
          box(
            title = "The Most Frequent Eurovision Hosts",
            width = "100%",
            height= 475,
            girafeOutput("map")
          )
        ),
        column(
          width=4,
          box(
            title = "Winners And Their Songs",
            width = "100%",
            height= 475,
            highchartOutput("winners")
          )
        ),
        column(
          width=4,
          box(
            title = "The Most Frequent Eurovision Song Languages",
            width = "100%",
            height= 475,
            girafeOutput("circles")
          )
        ),
      column(
        width=8,
        box(sliderInput("years_end", "Select the Endpoint of Data:", 
                    min = min(Years_list),
                    max = max(Years_list),
                    value = max(Years_list), 
                    step = 1,
                    #animate=TRUE,
                    sep = ""),
            width="100%")
      ),
      column(
        width=4,
        box(sliderInput("circles_percentage", "Select Minimum Percentage:", 
                        min = 1,
                        max = 10,
                        value = 2.8, 
                        step = 0.1,
                        pre = "", post = "%"),
            width="100%")
      ))
      ),
    tabItem(
      tabName = "most_points",
      includeCSS("www/style.css"),
      fluidRow(column(
        width = 10,
        div(
          style = "display: flex; align-items: baseline; justify-content: left; margin-top: 1px;",
          div("EUROVISION", style = "color: #FFF400; font-size: 30px; font-weight: 700; font-family: 'Cantarell', sans-serif; margin-right: 10px;"),
          div("STATISTICS", style = "color: white; font-size: 30px; font-weight: 400; font-family: 'Cantarell', sans-serif;")
        )
      )),
      fluidRow(
        column(
          width = 4,
          box(selectInput(
            "select",
            "Select a time-period:",
            choices = list("1975 - 2003", "2004 - 2015", "2016 - 2024"),
            selected = c(1975, 2003)
          ),
          height = 180,
          width= 70),
          box(title = "Voting Format Description",
            textOutput("description"),
            width=70)
          
        ),
        column(
          width = 6,
          box(title = "Most Points Received",
              width = 12,
              girafeOutput("points"))
        ),
        column(
          width = 2
        )
      )
    ),
    tabItem(
      tabName = "placements",
      includeCSS("www/style.css"),
      fluidRow(column(
        width = 10,
        div(
          style = "display: flex; align-items: baseline; justify-content: left; margin-top: 1px;",
          div("EUROVISION", style = "color: #FFF400; font-size: 30px; font-weight: 700; font-family: 'Cantarell', sans-serif; margin-right: 10px;"),
          div("STATISTICS", style = "color: white; font-size: 30px; font-weight: 400; font-family: 'Cantarell', sans-serif;")
        )
      )),
      fluidRow(
        column(
          width = 4,
          box(
            selectInput("countries", "Select Countries:", choices = unique(Countries_list),
                        selected="Poland", multiple = TRUE),
            sliderInput("years", "Select Year Range:", 
                        min = min(Years_list),
                        max = max(Years_list),
                        value = c(min(Years_list), 
                                  max(Years_list)), 
                        step = 1, 
                        sep = ""),
            sliderInput("places", "Select Placement Range:", 
                        min = min(Places_list),
                        max = max(Places_list),
                        value = c(min(Places_list), 
                                  max(Places_list)), 
                        step = 1, sep = ""),
            height = 300,
            width = "100%"
          )),
        column(
          width=6,
          box(title = "Change of Placements Over Time",
              width = "100%",
              girafeOutput("placements_plot")
              )
        ),
        column(
          width = 2
        ))
    ),
    tabItem(
      tabName = "participation",
      includeCSS("www/style.css"),
      fluidRow(column(
        width = 10,
        div(
          style = "display: flex; align-items: baseline; justify-content: left; margin-top: 1px;",
          div("EUROVISION", style = "color: #FFF400; font-size: 30px; font-weight: 700; font-family: 'Cantarell', sans-serif; margin-right: 10px;"),
          div("STATISTICS", style = "color: white; font-size: 30px; font-weight: 400; font-family: 'Cantarell', sans-serif;")
        )
      )),
      fluidRow(column(width = 4,
                      box(
                        checkboxGroupInput(
                          "continent",
                          "Select Continents: ",
                          choices = c(
                            "Europe" = "europe",
                            "Asia" = "asia",
                            "Africa" = "africa",
                            "Oceania" = "oceania"
                          ),
                          selected = c("europe", "asia", "africa", "oceania")
                        ),
                        width = "100%",
                      )),
               column(width=8,
                      box(title = "Countries Participation Datatable",
                          width = "100%",
                          reactableOutput("countries_table")
                      )))
    ),
    tabItem(
      tabName = "about",
      includeCSS("www/style.css"),
      fluidRow(column(
        width = 10,
        div(
          style = "display: flex; align-items: baseline; justify-content: left; margin-top: 1px;",
          div("EUROVISION", style = "color: #FFF400; font-size: 30px; font-weight: 700; font-family: 'Cantarell', sans-serif; margin-right: 10px;"),
          div("STATISTICS", style = "color: white; font-size: 30px; font-weight: 400; font-family: 'Cantarell', sans-serif;")
        )
      )),
      fluidRow(
        column(12, align = "center",
               h3("Welcome to EuroViz'on - Eurovision Statistics Visualization Dashboard!"),
               p("This interactive dashboard visualizes some of the most intriguing statistics from past Eurovision Song Contests. If you're an Eurovision fan or simply love data and graphs, you've found the right place! The name EuroViz'on is a play on the words Eurovision and Visualization (Viz) – the two things we love the most! The dashboard's theme is inspired by the vibrant and colorful theming of the ESC.", style = "text-align:justify;"),
               h4("Main Page"),
               p("On the main page, you'll find visualizations that depict general information, including the most frequent Eurovision host, the most used language in Eurovision songs, the most often winning country, and their winning songs. Don't hesitate to hover your mouse pointer over the visualizations to see more detailed information!", style = "text-align:justify;"),
               h4("Most Points Received"),
               p("There, you will find the top ten highest-scoring songs from different time periods. We categorized songs this way due to the varying number of participants and point-awarding systems used each year, aiming to make them as comparable as possible. For years before 1975, such analysis is not applicable due to the voting system, or lack thereof, used before then. Once again, don't hesitate to hover and see more!", style = "text-align:justify;"),
               h4("Country Placements"),
               p("On this page, you'll see a plot showcasing how a country's placement changes over the years. Choose the countries you want to analyze and compare, select a year range, and the placement range that interests you. Hover over the country flag on the plot to find out which song and artist achieved that placement for their country.", style = "text-align:justify;"),
               h4("Country Participation Details"),
               p("There, you'll find an efficient way to see interesting facts about each country's participation, from the year they joined the contest to their best song in Eurovision (based on placement and points in case of ties). Select the continent you're interested in.", style = "text-align:justify;"),
               h4("We hope you enjoy!"),
               p("Julia and Marcel", style = "text-align:center; font-weight:bold;"),
               h5("Sources of our data:", style = "text-align:left;"),
               p(tags$a(href = "https://eschome.net/", "https://eschome.net/"), style = "text-align:left;"),
               p(tags$a(href = "https://escincontext.com/resources/data/", "https://escincontext.com/resources/data/"), style = "text-align:left;"),
               p(tags$a(href = "https://docs.google.com/spreadsheets/d/1UUXinsHP4iDUwprM_KKEng4DBK2uC7Y1NdbnD1lmkSU/edit#gid=0", "https://docs.google.com/spreadsheets/d/1UUXinsHP4iDUwprM_KKEng4DBK2uC7Y1NdbnD1lmkSU/edit#gid=0"), style = "text-align:left;"),
               p("*some of the data was added by us manually (2024 contest results)", style = "text-align:left;")
        )
      )
    )
    
  ))
)
