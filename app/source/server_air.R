## server_air.R: air quality data

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

# Air quality data is loaded for each user session, so that it is up to date (the source data is updated daily)

try(air <- importAURN(site = "ED3", year = 2015:2016, pollutant = c("pm10", "pm2.5", "o3", "no2", "wd", "ws")))
if (exists("air") && !is.null(air) && (nrow(air) > 0)) {
  try(air_latest <- tail(air[(!is.na(air$o3)), ], 1))
}

output$panel_air <- renderUI({
  if(!(input$show_panel_air))
    return()
  if(exists("air") && !is.null(air) && (nrow(air) > 0)) {
    list(
      radioButtons("pollutant", label = NULL,
                   choices = list("NO2" = "no2", "Ozone" = "o3", "PM10" = "pm10","PM2.5" = "pm2.5"), selected = "no2"),
      radioButtons("air_plot_type", label = NULL,
                   choices = list("time series plot" = "time","wind rose (slow)" = "rose"), selected = "time") 
    )
  } else {"No data currently available"}
})

output$panel_air_plot <- renderUI({
  if(!(input$show_panel_air) || !exists("air") || is.null(air) || (nrow(air) == 0))
    return()
  list(plotOutput("air_plot", width = 500, height = 400))
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_panel_air)) {
    proxy %>% clearGroup("air")
    return()
  }
  if(is.null(input$pollutant) || is.null(input$air_plot_type) || !exists("air") || is.null(air) || !exists("air_latest") || is.null(air_latest) || (nrow(air_latest) == 0))
    return()
  # For now, we only show data for Edinburgh St Leonards station
  if(input$show_panel_air) {
    current_station <- air_stations[air_stations$site == "ED3", ]
    popup <- paste(
      "<b>", current_station$name, "air quality station</b>", "<br>", 
      "<b>Most recent data: </b>", air_latest$date, "<br>", 
      "<b>NO2: </b>", round(air_latest$no2, 1), " &mu;g/m3 - EU limit: 40 (year), 200 (1h)<br>", 
      "<b>Ozone: </b>", round(air_latest$o3, 1), " &mu;g/m3 - EU target: 120 (daily 8 hour mean)<br>", 
      "<b>PM10: </b>", round(air_latest$pm10, 1), " &mu;g/m3 - EU limit: 40 (year) (Scotland: 18), 50 (1h)<br>", 
      "<b>PM2.5: </b>", round(air_latest$pm2.5, 1), " &mu;g/m3 - EU limit: 25 (year) (Scotland: 12)")
    proxy %>%
      clearGroup("air") %>% 
      addMarkers(data = current_station,
                 lng = ~lon, 
                 lat = ~lat, 
                 popup = popup, 
                 group = "air") %>% 
      addPopups(data = current_station,
                lng = ~lon, 
                lat = ~lat, 
                popup = popup,
                popupOptions(maxWidth = 500),
                group = "air")
  } else
    proxy %>% clearGroup("air")
  # Diplay air quality data using functions of the openair package
  if (input$air_plot_type == "time")
    output$air_plot <- renderPlot(smoothTrend(air, pollutant = input$pollutant, xlab = "Daily levels spread & monthly averages in 2015 & 2016 (microg/m3)", date.breaks = 12, ylim = c(0,max(air[,input$pollutant]))), width = 500, height = 400)
  if (input$air_plot_type == "rose")
    output$air_plot <- renderPlot(polarPlot(air, pollutant = input$pollutant, xlab = "Mean levels by wind speed and direction in 2015 & 2016 (microg/m3)"), width = 500, height = 400)
})
