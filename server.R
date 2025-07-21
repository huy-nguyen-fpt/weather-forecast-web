library(httr)
library(jsonlite)
library(leaflet)
library(plotly)

# Hàm: Lấy lat/lon từ tên thành phố
get_lat_lon_from_city <- function(city_name, api_key = "6df84480806d2c971ee2b92077fa5352") {
  if (is.null(city_name) || city_name == "") return(NULL)
  url <- paste0("http://api.openweathermap.org/geo/1.0/direct?q=", city_name, "&limit=1&appid=", api_key)
  response <- httr::GET(url)
  if (httr::http_type(response) != "application/json") return(NULL)
  json_result <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  if (length(json_result) == 0 || is.null(json_result$lat[1])) return(NULL)
  return(list(lat = json_result$lat[1], lon = json_result$lon[1]))
}

# Hàm: Lấy thời tiết hiện tại
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

# Hàm: Dự báo 7 ngày từ Visual Crossing
get_forecast_7days <- function(lat, lon, api_key = "8NWNBTS5ZSHQ6AWQ3XGUMP36H") {
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/", lat, ",", lon)
  query <- list(unitGroup = "metric", include = "days,current", key = api_key, contentType = "json")
  response <- GET(url, query = query)
  if (http_type(response) != "application/json") stop("Failed to fetch forecast.")
  json_data <- content(response, as = "parsed")
  forecast_list <- json_data$days
  lapply(forecast_list[1:7], function(day) {
    data.frame(
      city = json_data$address,
      lat = json_data$latitude,
      lon = json_data$longitude,
      date = day$datetime,
      temp = day$temp,
      temp_min = day$tempmin,
      temp_max = day$tempmax,
      humidity = day$humidity,
      pressure = day$pressure,
      wind_speed = day$windspeed,
      wind_dir = day$winddir,
      conditions = day$conditions,
      icon = day$icon
    )
  }) %>% bind_rows()
}

# SERVER
server <- function(input, output, session) {
  forecast_data_val <- reactiveVal()
  forecast_city_name <- reactiveVal("")  
  
  coord <- reactive({
    get_lat_lon_from_city(input$location)
  })
  
  observe({
    coord_val <- coord()
    if (!is.null(coord_val)) {
      data <- get_forecast_7days(coord_val$lat, coord_val$lon)
      forecast_data_val(data)
      forecast_city_name(input$location) 
    }
  })
  
  observeEvent(input$map_forecast_click, {
    click <- input$map_forecast_click
    if (!is.null(click)) {
      data <- get_forecast_7days(click$lat, click$lng)
      forecast_data_val(data)
      forecast_city_name(data$city[1])  
    }
  })
  
  weather_data <- reactive({
    coord_val <- coord()
    if (!is.null(coord_val)) get_weather_current(coord_val$lat, coord_val$lon)
  })
  
  # Click bản đồ hiển thị thời tiết hiện tại
  observeEvent(input$map_current_click, {
    click <- input$map_current_click
    if (is.null(click)) return()
    lat <- click$lat
    lon <- click$lng
    data <- get_weather_current(lat, lon)
    
    output$city <- renderText({ data$city })
    output$date <- renderText({ as.character(data$date) })
    output$temperature <- renderText({ paste0(data$temp, " °C") })
    output$feelsLikeText <- renderText({ paste0(data$feels_like, " °C") })
    output$humidityText <- renderText({ paste0(data$humidity, " %") })
    output$windSpeedText <- renderText({ paste0(data$wind_speed, " km/h") })
    output$pressureText <- renderText({ paste0(data$pressure, " hPa") })
    output$Tempmin <- renderText({ paste0(data$temp_min, " °C") })
    output$Tempmax <- renderText({ paste0(data$temp_max, " °C") })
    output$visibility <- renderText({ paste0(data$visibility, " m") })
    output$wind_deg <- renderText({ paste0(data$wind_deg, "°") })
    output$weather_main <- renderText({ data$weather_main })
    output$weather_description <- renderText({ data$weather_description })
    
    output$map_current <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lat = lat, lng = lon, zoom = 10) %>%
        addMarkers(lat = lat, lng = lon, popup = paste0("Temp: ", data$temp, "°C"))
    })
  })
  
  # Bản đồ hiện tại
  output$map_current <- renderLeaflet({
    data <- weather_data()
    if (is.null(data)) return(NULL)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = data$lon, lat = data$lat, zoom = 10) %>%
      addMarkers(lng = data$lon, lat = data$lat, popup = paste0("Temp: ", data$temp, "°C"))
  })
  
  #  Tên địa danh hiển thị chính xác
  output$forecast_city <- renderText({
    forecast_city_name()
  })
  
  output$map_forecast <- renderLeaflet({
    data <- forecast_data_val()
    if (is.null(data)) return(NULL)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = data$lat[1], lng = data$lon[1], zoom = 10) %>%
      addMarkers(lat = data$lat[1], lng = data$lon[1],
                 popup = paste0("Forecast Day 1: ", data$temp[1], "°C"))
  })
  
  output$forecast_chart <- renderPlotly({
    data <- forecast_data_val()
    req(data)
    
    var <- switch(input$forecast_metric,
                  "Temperature" = "temp",
                  "Humidity" = "humidity",
                  "Wind Speed" = "wind_speed",
                  "Pressure" = "pressure")
    
    title <- input$forecast_metric
    
    plot_ly(data, x = ~date, y = data[[var]], type = "scatter", mode = "lines+markers",
            name = title, line = list(color = 'royalblue')) %>%
      layout(title = paste("7-Day Forecast Chart:", title),
             xaxis = list(title = "Date"),
             yaxis = list(title = title))
  })
}
