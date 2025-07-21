# library(shiny)
# library(shinydashboard)
# library(readr)
# library(ggplot2)
# library(dplyr)
# library(glue)
# library(leaflet)
# library(lubridate)
# library(httr)
# library(jsonlite)
# library(shinythemes)
# library(tidyverse)

# ////////////////////////////////////////////////////////////////////////////
# UI
# ////////////////////////////////////////////////////////////////////////////
ui <- dashboardPage(
  skin = "blue",  # màu giao diện
  dashboardHeader(title = "Weather"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Current Weather", tabName = "current", icon = icon("cloud-sun")),
      menuItem("Forecast Weather", tabName = "forecast", icon = icon("cloud-sun"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Tab 1: Current Weather
      tabItem(tabName = "current",
              
              fluidRow(
                column(width = 6,
                       textInput("location", "Enter City Name:", value = "Hanoi"),
                       h1("Current Weather"),
                       h3("City:"), textOutput("city"),
                       h4("Date:"), textOutput("date"),
                       h4(icon("thermometer-half"), "Temperature:"), 
                       h2(textOutput("temperature")),
                       br(),
                       
                       fluidRow(
                         column(width = 6,
                                box(width = 12, 
                                    title = "Feels Like", 
                                    solidHeader = TRUE, 
                                    status = "primary", 
                                    textOutput("feelsLikeText"))
                         ),
                         column(width = 6,
                                box(width = 12, 
                                    title = "Humidity", 
                                    solidHeader = TRUE, 
                                    status = "warning", 
                                    textOutput("humidityText"))
                         )
                       ),
                       
                       fluidRow(
                         column(width = 6,
                                box(width = 12, 
                                    title = "Wind Speed", 
                                    solidHeader = TRUE, 
                                    status = "success", 
                                    textOutput("windSpeedText"))
                         ),
                         column(width = 6,
                                box(width = 12, 
                                    title = "Pressure", 
                                    solidHeader = TRUE, 
                                    status = "warning", 
                                    textOutput("pressureText"))
                         )
                       ),
                       
                       fluidRow(
                         column(width = 6,
                                box(width = 12, 
                                    title = "Temp Min", 
                                    solidHeader = TRUE, 
                                    status = "primary", 
                                    textOutput("Tempmin"))
                         ),
                         column(width = 6,
                                box(width = 12, 
                                    title = "Temp Max", 
                                    solidHeader = TRUE, 
                                    status = "primary", 
                                    textOutput("Tempmax"))
                         )
                       ),
                       
                       fluidRow(
                         column(width = 6,
                                box(width = 12, 
                                    title = "Visibility", 
                                    solidHeader = TRUE, 
                                    status = "success", 
                                    textOutput("visibility"))
                         ),
                         column(width = 6,
                                box(width = 12, 
                                    title = "Wind Degree", 
                                    solidHeader = TRUE, 
                                    status = "success", 
                                    textOutput("wind_deg"))
                         )
                       ),
                       
                       fluidRow(
                         column(width = 6,
                                box(width = 12, 
                                    title = "Weather Main", 
                                    solidHeader = TRUE, 
                                    status = "info", 
                                    textOutput("weather_main"))
                         ),
                         column(width = 6,
                                box(width = 12, 
                                    title = "Weather Description", 
                                    solidHeader = TRUE, 
                                    status = "info", 
                                    textOutput("weather_description"))
                         )
                       )
                ),
                
                column(width = 6,
                       box(width = 12,
                           title = "Map Location",
                           solidHeader = TRUE,
                           status = "info",
                           leafletOutput("map_current", height = "400px")
                       )
                )
              )
      ),
      
      # Tab 2: Forecast Weather
      tabItem(tabName = "forecast",
              
              # Tiêu đề + Địa danh + Chọn chỉ số
              fluidRow(
                column(width = 6,
                       h2("7-Day Weather Forecast"),
                       h4("Location Selected:"),
                       h3(textOutput("forecast_city"))  # cập nhật khi user nhập hoặc click bản đồ
                ),
                column(width = 6,
                       selectInput("forecast_metric", "Select Metric to View:",
                                   choices = c("Temperature", "Humidity", "Wind Speed", "Pressure"),
                                   selected = "Temperature")
                )
              ),
              
              # Biểu đồ Plotly
              fluidRow(
                box(width = 12,
                    title = "Forecast Chart (Next 7 Days)",
                    solidHeader = TRUE,
                    status = "primary",
                    plotlyOutput("forecast_chart", height = "400px")
                )
              ),
              
              # Bản đồ Leaflet
              fluidRow(
                box(width = 12,
                    title = "Forecast Location on Map",
                    solidHeader = TRUE,
                    status = "info",
                    leafletOutput("map_forecast", height = "400px")
                )
              )
      )
      
      
      
    )
  )
)