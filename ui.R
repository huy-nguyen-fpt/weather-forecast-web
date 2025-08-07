library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(dplyr)
library(glue)
library(leaflet)
library(lubridate)
library(httr)
library(jsonlite)
library(shinythemes)
library(tidyverse)
library(plotly)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Weather App"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Current Weather", tabName = "current", icon = icon("cloud-sun")),
                menuItem("7-Day Forecast", tabName = "forecast", icon = icon("calendar-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(".content-wrapper {
        background: url('clouds.gif') no-repeat center center fixed;
        background-size: cover;
      }
      .right-side {
        background-color: transparent !important;
      }
      .skin-blue .main-header .logo,
      .main-header .navbar {
        background-color: rgba(0, 0, 0, 0.3) !important;
      }
      .sidebar {
        background-color: rgba(255, 255, 255, 0.95) !important;
      }"))
    ),
    
    # ROW 1: LEFT - tab content, RIGHT - map
    fluidRow(
      column(width = 6,
             conditionalPanel(
               condition = "input.tabs == 'current'",
               h3("City:", style = "color: white; font-weight: bold; font-size: 22px;"),
               span(textOutput("city"), style = "color: white; font-size: 20px; font-weight: 500;"),
               
               h3("Date:", style = "color: white; font-weight: bold; font-size: 22px; margin-top: 10px;"),
               span(textOutput("date"), style = "color: white; font-size: 20px; font-weight: 500;"),
               
               h3(icon("thermometer-half"), "Temperature:", style = "color: white; font-weight: bold; font-size: 22px; margin-top: 10px;"),
               h1(textOutput("temperature"), style = "color: white; font-size: 40px; font-weight: bold; margin-bottom: 20px;"),
               tags$style("#temperature { color: white; }")
               ,
               column(width = 6,
                      box(title = tagList(icon("temperature-low"), "Feels Like Temperature"), status = "primary", solidHeader = TRUE, width = 12, h4(textOutput("feelsLikeText"))),
                      box(title = tagList(icon("wind"), "Wind Speed (km/h)"), status = "success", solidHeader = TRUE, width = 12, h4(textOutput("windSpeedText"))),
                      box(title = tagList(icon("temperature-low"), "Lowest Temperature"), status = "primary", solidHeader = TRUE, width = 12, h4(textOutput("Tempmin"))),
                      box(title = tagList(icon("eye"), "Visibility Distance (km)"), status = "success", solidHeader = TRUE, width = 12, h4(textOutput("visibility"))),
                      box(title = tagList(icon("cloud"), "Weather Condition"), status = "info", solidHeader = TRUE, width = 12, h4(textOutput("weather_main")))
               ),
               
               column(width = 6,
                      box(title = tagList(icon("tint"), "Humidity (%)"), status = "warning", solidHeader = TRUE, width = 12, h4(textOutput("humidityText1"))),
                      box(title = tagList(icon("tachometer-alt"), "Atmospheric Pressure (hPa)"), status = "warning", solidHeader = TRUE, width = 12, h4(textOutput("pressureText"))),
                      box(title = tagList(icon("temperature-high"), "Highest Temperature"), status = "primary", solidHeader = TRUE, width = 12, h4(textOutput("Tempmax"))),
                      box(title = tagList(icon("compass"), "Wind Direction (°)"), status = "success", solidHeader = TRUE, width = 12, h4(textOutput("wind_deg"))),
                      box(title = tagList(icon("align-left"), "Weather Description"), status = "info", solidHeader = TRUE, width = 12, h4(textOutput("weather_description")))
               ),
               box(title = "Chart Mode", status = "warning", solidHeader = TRUE, width = 12,
                   radioButtons("chart_mode", label = NULL,
                                choices = c("Time Series Charts" = "time", "Correlation Charts" = "correlation"),
                                selected = "time", inline = TRUE))
             ),
             conditionalPanel(
               condition = "input.tabs == 'forecast'",
               box(title = "7-Day Forecast", status = "primary", width = 12,
                   selectInput("forecast_metric", label = NULL,
                               choices = c("Temperature", "Humidity", "Wind Speed", "Pressure",
                                           "Temp Max", "Temp Min", "Feels Like", "Cloud Cover",
                                           "UV Index", "Precipitation", "Visibility")),
                   plotlyOutput("forecast_chart", height = "400px")
               )
             )
      ),
      column(width = 6,
             box(title = "Map (Click to update location)", status = "info", solidHeader = TRUE,
                 width = 12, leafletOutput("map", height = "800px"))
      )
    ),
    
    # ROW 2: 4 subplots only in tab 'current'
    conditionalPanel(
      condition = "input.tabs == 'current' && input.chart_mode == 'time'",
      fluidRow(
        column(width = 6, box(title = "Temperature Forecast", status = "primary", width = 12, plotlyOutput("plot_temp", height = "250px"))),
        column(width = 6, box(title = "Humidity Forecast", status = "primary", width = 12, plotlyOutput("plot_humidity", height = "250px")))
      ),
      fluidRow(
        column(width = 6, box(title = "Wind Speed Forecast", status = "primary", width = 12, plotlyOutput("plot_wind", height = "250px"))),
        column(width = 6, box(title = "Pressure Forecast", status = "primary", width = 12, plotlyOutput("plot_pressure", height = "250px")))
      )
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'current' && input.chart_mode == 'correlation'",
      fluidRow(
        column(width = 6, box(title = "Temperature vs Humidity", status = "info", width = 12, plotlyOutput("corr_temp_humid", height = "250px"))),
        column(width = 6, box(title = "Wind Speed vs Pressure", status = "info", width = 12, plotlyOutput("corr_wind_pressure", height = "250px")))
      ),
      fluidRow(
        column(width = 6, box(title = "Temperature vs Pressure", status = "info", width = 12, plotlyOutput("corr_temp_pressure", height = "250px"))),
        column(width = 6, box(title = "Humidity vs Wind Speed", status = "info", width = 12, plotlyOutput("corr_humid_wind", height = "250px")))
      )
    )
  )
)

