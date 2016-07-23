## server_osm.R: search OpenStreetMap data

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$osm_message <- renderUI(HTML("Enter a search term below"))

node_results <- 0
way_results <- 0
results_displayed <- 0

output$panel_osm <- renderUI({
  if(!(input$show_panel_osm))
    return()
  list(
    htmlOutput("osm_message"),
    textInput("osm_search_term", label = "", value = "", width = "180px"),
    radioButtons("osm_search_type", label = "", choices = list("amenities" = "amenity", "shops" = "shop"), selected = "amenity", width = "100%"),
    actionButton("osm_search_button", strong("search"), style="padding:6px; font-size:12.5px;width:57px"),
    actionButton("osm_clear_button", strong("clear"), style="padding:6px; font-size:12.5px;width:57px"),
    HTML("<br><br>The search will take a few seconds.<br><br>Possible search terms include:<br><br>Amenities: <b>pub, bar, cafe, cinema,<br>restaurant, fast food, market,<br>taxi rank, petrol station, charging station,<br>cash machine, bank, post office, post box,<br>kindergarten, chemist, vet,<br>toilets, drinking water,<br>police station, fire station, embassy.<br><a href = 'http://wiki.openstreetmap.org/wiki/Key:amenity' target='_blank'>Full list</a><br><br></b>Shops: <b> bakery, butcher, cheese,<br>fishmonger, optician, bike,<br> clothes, shoes, hairdresser, charity.<br><a href = 'http://wiki.openstreetmap.org/wiki/Key:shop' target='_blank'>Full list</a></b>")
  )
})

observeEvent(input$osm_search_button, {
  
  if(str_length(input$osm_search_term) > 0) {
    
    # For some reason the message below isn't rendered
    output$osm_message <- renderUI(HTML("Searching..."))
    
    try({
    input$osm_search_term %>%
      str_trim() %>%
      str_replace_all(" ", "_") %>%
      str_to_lower() %>%
      plural_to_singular(exceptions = c("toilets", "doctors", "clothes", "shoes")) %>%
      str_replace("_shop$", "") %>%
      replace_synonyms(synonyms_original, synonyms_converted) -> query_string
    query_string %>% osm_query(input$osm_search_type, "node") %>%
      overpass_query() -> osm_node
    })
    
    try({
    query_string %>% osm_query(input$osm_search_type, "way") %>%
      overpass_query() -> osm_way
    })
      
    if((!exists("osm_node") || is.null(osm_node) || (nrow(osm_node) == 0)) && (!exists("osm_way") || is.null(osm_way) || (nrow(osm_way) == 0)))
      output$osm_message <- renderUI(HTML("<span style='color:red'><b>No data found</b></span>")) 
    else {
      
      if(exists("osm_node") && !is.null(osm_node) && (nrow(osm_node) > 0)) {
        
        node_results <- nrow(osm_node)
        
        osm_node_names_present <- intersect(all_osm_names, names(osm_node))
        osm_node@data$popup <- ""
        for(osm_name in osm_node_names_present) {
          osm_node@data$popup <- paste(sep = "", osm_node@data$popup, ifelse(is.na(osm_node@data[,osm_name]), "", paste("<b>", osm_name, ":</b> ", osm_node@data[,osm_name], "<br>")))
        }
        
        if(exists("osm_way") && !is.null(osm_way) && (nrow(osm_way) > 0)) {
          
          way_results <- nrow(osm_way)
          
          osm_way <- SpatialLinesDataFrame(SpatialLines(unlist(osm_way@lines, use.names = FALSE)),osm_way@data) 
          
          osm_way_names_present <- intersect(all_osm_names, names(osm_way))
          osm_way@data$popup <- ""
          for(osm_name in osm_way_names_present) {
            osm_way@data$popup <- paste(sep = "", osm_way@data$popup, ifelse(is.na(osm_way@data[,osm_name]), "", paste("<b>", osm_name, ":</b> ", osm_way@data[,osm_name], "<br>")))
          }
          
        }
        
        current_group <- "osm"    
        proxy <- leafletProxy("mymap")
        
        #proxy %>% clearGroup(current_group) 
        
        results_displayed <<- results_displayed + 1
        
        if(exists("osm_node") && !is.null(osm_node) && (nrow(osm_node) > 0))
          proxy %>% addCircleMarkers(data = osm_node,
                                     lng = ~lon,
                                     lat = ~lat,
                                     popup = ~popup,
                                     radius = 5,
                                     col = osm_palette[results_displayed %% length(osm_palette)],
                                     opacity = 0.95,
                                     fillOpacity = 0.2,
                                     group = current_group)
        
        if(exists("osm_way") && !is.null(osm_way) && (nrow(osm_way) > 0))
          proxy %>% addPolygons(data = osm_way,
                                popup = ~popup,
                                col = osm_palette[results_displayed %% length(osm_palette)],
                                weight = 4,
                                opacity = 0.95,
                                fillOpacity = 0.2,
                                group = current_group)
        
        output$osm_message <- renderUI(HTML(paste("Found: ", node_results, " points, ", way_results, " shapes.", sep = "")))
        
      }}
  }
})

observeEvent(input$osm_clear_button, {
  proxy <- leafletProxy("mymap")
  proxy %>% clearGroup("osm")
  output$osm_message <- renderUI(HTML("Enter a search term below"))
  results_displayed <<- 0
})
