## server_amenities.R:  allotments, recycling

## MIT License
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

# Initialise output messages

output$recycling_message <- renderUI(HTML("(at high zoom only)"))

output$panel_amenities <- renderUI({
  if(!(input$show_panel_amenities))
    return()
  list(
    checkboxInput("show_allotments", label = "Allotments", width = "100%"),
    checkboxGroupInput("show_recycling", label = "Recycling", width = "100%", choices = list("packaging", "paper", "glass", "textile", "compost", "plastic", "book", "cans" = "can")),
    htmlOutput("recycling_message")
  )
})  

observe({
  proxy <- leafletProxy("mymap")
  
  if ((length(input$show_recycling) == 0) || (input$mymap_zoom <= 15))
    proxy %>% clearGroup(all_recycling_categories)
  
  if ((length(input$show_recycling) > 0) && (input$mymap_zoom > 15)) {
    
    # Subset the recycling dataset to the current view, to keep the app responsive
    recycling_filtered <- recycling[((recycling$lon < input$mymap_bounds$east) & (recycling$lon > input$mymap_bounds$west) & (recycling$lat > input$mymap_bounds$south) & (recycling$lat < input$mymap_bounds$north)), ]
    
    recycling_filtered <- recycling_filtered[recycling_filtered$cat %in% input$show_recycling, ]
    
    output$recycling_message <- renderUI(HTML(""))
    
    proxy %>% addCircleMarkers(data = recycling_filtered,
                               lng = ~lon,
                               lat = ~lat,
                               popup = ~paste(sep = "",
                                              "<b>Recycling point:</b>",
                                              "<br>", "<b>", site, "</b>", " (", location, ")", 
                                              "<br>", "<b>Type:</b> ", type 
                               ), 
                               radius = ~7*n, 
                               col = ~colour, 
                               opacity = 0.95, 
                               fillOpacity = 0, 
                               group = ~cat)
    
    groups_displayed <<- union(groups_displayed, input$show_recycling)
    
    proxy %>% clearGroup(setdiff(all_recycling_categories, input$show_recycling))
    
  } else {
    if (length(input$show_recycling) > 0)
      output$recycling_message <- renderUI(HTML("<span style='color:red'><b>(zoom in further)</b></span>"))
    else output$recycling_message <- renderUI(HTML("(at high zoom only)"))
  }
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_allotments)) {
    proxy %>% clearGroup("allotments")
    return()
  }
  current_group <- "allotments"
  if (input$show_allotments) {
    proxy %>% addCircleMarkers(data = allotments,
                               lng = ~lon, 
                               lat = ~lat, 
                               popup = ~paste(sep = "", 
                                              "<b>", Name, "</b>", " (", Address, ", ", Postcode, ")",
                                              "<br>", "<b>Telephone:</b> ", Telephone,
                                              "<br>", "<b>Email:</b> ", Email,
                                              "<br>", "<b>Plots:</b> ", `Total plots`, ", <b>Vacant plots:</b> ", `Vacant plots`,
                                              "<br>", "<b>Waiting time:</b> ", `Waiting time`,
                                              "<br>", "<b>Facilities:</b> ", Facilities
                               ), 
                               radius = ~sqrt(plots*3), 
                               col = ~colorNumeric(palette = "RdYlGn", domain = -wait)(-wait), 
                               opacity = 0.95, 
                               fillOpacity = 0.2, 
                               group = current_group)
  } else
    proxy %>% clearGroup(current_group)
})
