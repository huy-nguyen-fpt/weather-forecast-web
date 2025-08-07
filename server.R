library(httr)
library(jsonlite)
library(leaflet)
library(plotly)
library(shiny)
library(dplyr)

get_weather_current <- function(lat, lon, api_key = "6df84480806d2c971ee2b92077fa5352") {
  url <- "https://api.openweathermap.org/data/2.5/weather"
  query <- list(lat = lat, lon = lon, appid = api_key, units = "metric")
  response <- GET(url, query = query)
  if (http_type(response) != "application/json") stop("Failed to fetch current weather.")
  json <- content(response, as = "parsed")
  data.frame(
    city = json$name,
    date = Sys.Date(),
    temp = json$main$temp,
    feels_like = json$main$feels_like,
    temp_min = json$main$temp_min,
    temp_max = json$main$temp_max,
    pressure = json$main$pressure,
    humidity = json$main$humidity,
    visibility = json$visibility,
    wind_speed = round(json$wind$speed * 3.6, 2),
    wind_deg = json$wind$deg,
    weather_main = json$weather[[1]]$main,
    weather_description = json$weather[[1]]$description,
    lat = json$coord$lat,
    lon = json$coord$lon
  )
}

get_forecast_7days <- function(lat, lon, api_key = "8NWNBTS5ZSHQ6AWQ3XGUMP36H") {
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/", lat, ",", lon)
  query <- list(unitGroup = "metric", include = "days", key = api_key, contentType = "json")
  response <- GET(url, query = query)
  json_data <- content(response, as = "parsed")
  forecast_list <- json_data$days
  lapply(forecast_list[1:7], function(day) {
    data.frame(
      date = day$datetime,
      temp = day$temp,
      temp_min = day$tempmin,
      temp_max = day$tempmax,
      feelslike = day$feelslike,
      humidity = day$humidity,
      pressure = day$pressure,
      wind_speed = day$windspeed,
      cloudcover = day$cloudcover,
      precip = day$precip,
      uvindex = day$uvindex,
      visibility = day$visibility
    )
  }) %>% bind_rows()
}

# ==== SERVER ====
server <- function(input, output, session) {
  lat_default <- 21.03
  lon_default <- 105.85
  
  clicked_coords <- reactiveVal()
  weather_data_val <- reactiveVal()
  forecast_data_val <- reactiveVal()
  city_name_val <- reactiveVal()
  
  observe({
    weather <- get_weather_current(lat_default, lon_default)
    forecast <- get_forecast_7days(lat_default, lon_default)
    
    clicked_coords(list(lat = lat_default, lon = lon_default))
    weather_data_val(weather)
    forecast_data_val(forecast)
    city_name_val(weather$city)
  })
  
  # ==== XỬ LÝ CLICK ====
  observeEvent(input$map_click, {
    click <- input$map_click
    req(click)
    
    lat <- click$lat
    lon <- click$lng
    clicked_coords(list(lat = lat, lon = lon))
    
    weather <- get_weather_current(lat, lon)
    forecast <- get_forecast_7days(lat, lon)
    
    weather_data_val(weather)
    forecast_data_val(forecast)
    city_name_val(weather$city)
    
    cat(" Clicked:", lat, lon, "\n")
    cat(" Thành phố:", weather$city, "\n")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = lat_default, lng = lon_default, zoom = 6)
  })
  
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  # ==== CẬP NHẬT MARKER ====
  observeEvent(weather_data_val(), {
    data <- weather_data_val()
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lat = data$lat, lng = data$lon,
                 popup = paste0("Temp: ", data$temp, "°C"))
  })
  
  
  output$forecast_city <- renderText({ city_name_val() })
  
  # ==== OUTPUT - Current Weather ====
  output$city <- renderText({ weather_data_val()$city })
  output$date <- renderText({ as.character(weather_data_val()$date) })
  output$temperature <- renderText({ paste0(weather_data_val()$temp, "°C") })
  output$feelsLikeText <- renderText({ paste0(weather_data_val()$feels_like, "°C") })
  output$humidityText1 <- renderText({ paste0(weather_data_val()$humidity, " %") })
  output$humidityText2 <- renderText({ paste0(weather_data_val()$humidity, " %") })
  output$pressureText <- renderText({ paste0(weather_data_val()$pressure, " hPa") })
  output$Tempmin <- renderText({ paste0(weather_data_val()$temp_min, "°C") })
  output$Tempmax <- renderText({ paste0(weather_data_val()$temp_max, "°C") })
  output$visibility <- renderText({ paste0(weather_data_val()$visibility, " m") })
  output$wind_deg <- renderText({ paste0(weather_data_val()$wind_deg, "°") })
  output$windSpeedText <- renderText({ paste0(weather_data_val()$wind_speed, " km/h") })
  output$weather_main <- renderText({ weather_data_val()$weather_main })
  output$weather_description <- renderText({ weather_data_val()$weather_description })
  
  # ==== OUTPUT - Forecast Chart ====
  output$forecast_chart <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    
    metric <- input$forecast_metric
    
    y_data <- switch(metric,
                     "Temperature" = data$temp,
                     "Humidity" = data$humidity,
                     "Wind Speed" = data$wind_speed,
                     "Pressure" = data$pressure,
                     "Temp Max" = data$temp_max,
                     "Temp Min" = data$temp_min,
                     "Feels Like" = data$feelslike,
                     "Cloud Cover" = data$cloudcover,
                     "UV Index" = data$uvindex,
                     "Precipitation" = data$precip,
                     "Visibility" = data$visibility
    )
    
    y_label <- switch(metric,
                      "Temperature" = "Temperature (°C)",
                      "Humidity" = "Humidity (%)",
                      "Wind Speed" = "Wind Speed (km/h)",
                      "Pressure" = "Pressure (hPa)",
                      "Temp Max" = "Max Temperature (°C)",
                      "Temp Min" = "Min Temperature (°C)",
                      "Feels Like" = "Feels Like (°C)",
                      "Cloud Cover" = "Cloud Cover (%)",
                      "UV Index" = "UV Index",
                      "Precipitation" = "Precipitation (mm)",
                      "Visibility" = "Visibility (km)")
    
    
    plot_ly(data, x = ~date, y = y_data, type = "scatter", mode = "lines+markers",
            name = metric, line = list(color = 'royalblue')) %>%
      layout(title = paste("7-Day Forecast for", city_name_val()),
             xaxis = list(title = "Date"),
             yaxis = list(title = y_label))
  })
  output$plot_temp <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~date, y = ~temp, type = "scatter", mode = "lines+markers",
            line = list(color = "red")) %>%
      layout(title = "Temperature Forecast", 
             xaxis = list(title = "Date"), 
             yaxis = list(title = "°C"))
  })
  
  output$plot_humidity <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~date, y = ~humidity, type = "scatter", mode = "lines+markers",
            line = list(color = "blue")) %>%
      layout(title = "Humidity Forecast", 
             xaxis = list(title = "Date"), 
             yaxis = list(title = "%"))
  })
  
  output$plot_wind <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~date, y = ~wind_speed, type = "scatter", mode = "lines+markers",
            line = list(color = "green")) %>%
      layout(title = "Wind Speed Forecast", 
             xaxis = list(title = "Date"), 
             yaxis = list(title = "km/h"))
  })
  
  output$plot_pressure <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~date, y = ~pressure, type = "scatter", mode = "lines+markers",
            line = list(color = "purple")) %>%
      layout(title = "Pressure Forecast", 
             xaxis = list(title = "Date"), 
             yaxis = list(title = "hPa"))
  })
  output$corr_temp_humid <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~temp, y = ~humidity, type = "scatter", mode = "markers",
            marker = list(color = 'red')) %>%
      layout(title = "Temp vs Humidity", xaxis = list(title = "Temperature (°C)"), yaxis = list(title = "Humidity (%)"))
  })
  
  output$corr_wind_pressure <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~wind_speed, y = ~pressure, type = "scatter", mode = "markers",
            marker = list(color = 'green')) %>%
      layout(title = "Wind Speed vs Pressure", xaxis = list(title = "Wind Speed (km/h)"), yaxis = list(title = "Pressure (hPa)"))
  })
  
  output$corr_temp_pressure <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~temp, y = ~pressure, type = "scatter", mode = "markers",
            marker = list(color = 'blue')) %>%
      layout(title = "Temp vs Pressure", xaxis = list(title = "Temperature (°C)"), yaxis = list(title = "Pressure (hPa)"))
  })
  
  output$corr_humid_wind <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    plot_ly(data, x = ~humidity, y = ~wind_speed, type = "scatter", mode = "markers",
            marker = list(color = 'purple')) %>%
      layout(title = "Humidity vs Wind Speed", xaxis = list(title = "Humidity (%)"), yaxis = list(title = "Wind Speed (km/h)"))
  })
  
}


