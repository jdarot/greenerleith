## server_search.R: address search

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$search_message <- renderUI(HTML("Enter address below"))

observeEvent(input$search_button, {
  
  if(!str_detect(input$search, fixed("Edinburgh", ignore_case = TRUE)) && !((str_to_upper(str_sub(input$search, 1, 2)) == "EH") && (str_length(str_replace_all(input$search, " ", "")) %in% c(6, 7))) ) suffix <- ", Edinburgh" else suffix <- ""
  # Use Google API for address search
  location <- geocode(paste(input$search, suffix, sep = ""), source = "google")
  # Do not show addresses beyond the LDP boundaries
  if ((!is.na(location$lat)) && (location$lat < max_lat) && (location$lat > min_lat) && (!is.na(location$lon)) && (location$lon < max_lon) && (location$lon > min_lon)) {
    proxy <- leafletProxy("mymap")
    popup = paste(sep = "",
                  "Location: ", input$search,
                  ifelse(
                    ((str_to_upper(str_sub(input$search, 1, 2)) == "EH") && (str_length(str_replace_all(input$search, " ", "")) %in% c(6, 7))), 
                    paste(sep="", "<b>", cleanURL(paste(sep = "", "https://www.writetothem.com/who?pc=", str_replace_all(input$search, " ", "")), "My elected representatives"), "</b>"), 
                    "<br>(Enter full postcode to see<br>your elected representatives)")
    )
    proxy %>% addMarkers(
      lng = location$lon,
      lat = location$lat,
      popup = popup,
      group = "search") %>%
      addPopups(
        lng = location$lon,
        lat = location$lat,
        popup = popup,
        group = "search") %>%
      setView(lng = location$lon, lat = location$lat, zoom = input$mymap_zoom)
    output$search_message <- renderUI(HTML("Enter address below"))
  } else {
    output$search_message <- renderUI(HTML("<span style='color:red'><b>Address not found, or outside LDP area.</b><span style='color:red'>"))
  }
})

observeEvent(input$clear_button, {
  proxy <- leafletProxy("mymap")
  proxy %>% clearGroup("search")
  output$search_message <- renderUI(HTML("Enter address below"))
})
