## server_osm.R: search OpenStreetMap data

## MIT License
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$osm_message <- renderUI(HTML("Enter a search term below"))

output$panel_osm <- renderUI({
  if(!(input$show_panel_osm))
    return()
  list(
    htmlOutput("osm_message"),
    textInput("osm_search_term", label = "", value = "", width = "180px"),
    radioButtons("osm_search_type", label = "", choices = list("amenities" = "amenity", "shops" = "shop"), selected = "amenity", width = "100%"),
    actionButton("osm_search_button", strong("search"), style="padding:6px; font-size:12.5px;width:57px"),
    actionButton("osm_clear_button", strong("clear"), style="padding:6px; font-size:12.5px;width:57px"),
    HTML("<br><br>The search will take a few seconds.<br><br>Possible search terms include:<br><br>Amenities: <b>pub, bar, cafe, cinema,<br>restaurant, fast food, market,<br>taxi rank, petrol station, charging station,<br>cash machine, bank, post office, post box,<br>kindergarten, chemist, vet,<br>toilets, drinking water,<br>police station, fire station, embassy.<br><a href = 'http://wiki.openstreetmap.org/wiki/Key:amenity' target='_blank'>Full list</a><br><br></b>Shops: <b> bakery, butcher, cheese,<br>fishmonger, optician, bike,<br> clothes, shoes, hairdresser, charity.<br><a href = 'http://wiki.openstreetmap.org/wiki/Key:shop' target='_blank'>Full list</a></b><br><br>Note: the search is in development<br>and the results might be incomplete.")
  )
})

observeEvent(input$osm_search_button, {
  
  if(str_length(input$osm_search_term) > 0) {
    
    # For some reason the message below isn't rendered
    output$osm_message <- renderUI(HTML("Searching..."))
    
    input$osm_search_term %>%
      str_trim() %>%
      str_replace_all(" ", "_") %>%
      str_to_lower() %>%
      plural_to_singular(exceptions = c("toilets", "doctors", "clothes", "shoes")) %>%
      str_replace("_shop$", "") %>%
      replace_synonyms(synonyms_original, synonyms_converted) %>%
      osm_query(input$osm_search_type, "node") %>%
      overpass_query() -> osm
    
    if(is.null(osm) || nrow(osm) == 0)
      output$osm_message <- renderUI(HTML("<span style='color:red'><b>No data found</b></span>")) 
    else {
      osm_names_present <- intersect(all_osm_names, names(osm))
      osm@data$popup <- ""
      for(osm_name in osm_names_present) {
        osm@data$popup <- paste(sep = "", osm@data$popup, ifelse(is.na(osm@data[,osm_name]), "", paste("<b>", osm_name, ":</b> ", osm@data[,osm_name], "<br>")))
      }
      
      current_group <- "osm"    
      proxy <- leafletProxy("mymap")
      proxy %>% clearGroup(current_group) %>%
        addCircleMarkers(data = osm,
                         lng = ~lon,
                         lat = ~lat,
                         popup = ~popup,
                         radius = 6,
                         col = "purple",
                         opacity = 0.95,
                         fillOpacity = 0.2,
                         group = current_group)
      
      output$osm_message <- renderUI(HTML("Enter a search term below"))
    }
  }
})

observeEvent(input$osm_clear_button, {
  proxy <- leafletProxy("mymap")
  proxy %>% clearGroup("osm")
  output$osm_message <- renderUI(HTML("Enter a search term below"))
})
