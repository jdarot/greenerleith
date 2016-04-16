## server_planning.R: planning framework

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

# Initialise messages
output$listed_buildings_message <- renderUI(HTML("(at high zoom only)"))
output$open_spaces_message <- renderUI(HTML("(at high zoom only)"))

output$panel_planning <- renderUI({
  if(!(input$show_panel_planning))
    return()
  list(
    checkboxGroupInput("show_ldp", label = "Local Development Plan", 
                       choices = list("proposed housing", "proposed green spaces","proposed schools")
    ),
    checkboxGroupInput("show_conservation", label = "Protected areas", 
                       choices = list("world heritage site", "conservation areas", "green belt", "tree preservation orders")
    ),
    checkboxInput("show_living_landscapes", "living landscapes", FALSE),
    checkboxInput("show_open_spaces", "open spaces", FALSE),
    htmlOutput("open_spaces_message"),
    checkboxInput("show_vacant", "vacant and derelict land (2014)", FALSE),
    checkboxInput("show_listed_buildings", "listed buildings", FALSE),
    htmlOutput("listed_buildings_message")
  )
})

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "proposed housing"
  if (current_group %in% input$show_ldp) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = housing_prop, 
                            popup = "LDP housing proposal", 
                            weight = 2, 
                            col = "red", 
                            fillOpacity = 0.1, 
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "proposed green spaces"
  if (current_group %in% input$show_ldp) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = green_prop, 
                            popup = "LDP green space proposal", 
                            weight = 2, col = "green", 
                            fillOpacity = 0.1, 
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "proposed schools"
  if (current_group %in% input$show_ldp) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addCircleMarkers(data = school_prop, 
                                 popup = "LDP school proposal", 
                                 radius = 6, 
                                 col = "black", 
                                 opacity = 0.95, 
                                 fillOpacity = 0.2, 
                                 group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
})

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "conservation areas"
  if (current_group %in% input$show_conservation) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = conservation_areas, 
                            popup = ~paste(sep = "",
                                           Name, " conservation area",
                                           properURL
                            ),
                            weight = 2, 
                            col = "black", 
                            fillOpacity = 0.1, 
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "world heritage site"
  if (current_group %in% input$show_conservation) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = world_heritage, 
                            popup = "world heritage site", 
                            weight = 2, 
                            col = "black", 
                            fillOpacity = 0.1, 
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "green belt"
  if (current_group %in% input$show_conservation) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = green_belt, 
                            popup = "green belt", 
                            weight = 2, 
                            col = "green", 
                            fillOpacity = 0.1, 
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "tree preservation orders"
  if (current_group %in% input$show_conservation) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = tree_orders, 
                            popup = ~paste(sep = "",
                                           "Tree preservation order",
                                           "<br>", "<b>Ref:</b> ", CONNAME
                            ), 
                            weight = 2, 
                            col = "green", 
                            fillOpacity = 0.1, 
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
})

observe({
  proxy <- leafletProxy("mymap")
  current_group <- "living_landscapes"
  if(is.null(input$show_living_landscapes)) {
    proxy %>% clearGroup(current_group)
    return()
  }
  if(input$show_living_landscapes) {
    proxy %>% addPolygons(data = living_landscapes, 
                          popup = ~paste(sep = "",
                                         "Living landscapes:",
                                         "<br>", "<b>Location:</b> ", Location,
                                         "<br>", "<b>Original state:</b> ", ExFeature,
                                         "<br>", "<b>Objective:</b> ", ProFeature,
                                         "<br>", "<b>Comments:</b> ", Comment
                          ), 
                          weight = 2, 
                          col = "green", 
                          opacity = 0.95, 
                          fillOpacity = 0.1,  
                          group = current_group)
  } else 
    proxy %>% clearGroup(current_group)
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_open_spaces)) {
    proxy %>% clearGroup("open_spaces")
    return()
  }
  current_group <- "open_spaces"
  # Only show open spaces at high zoom levels, to keep the app responsive
  if (input$show_open_spaces && (input$mymap_zoom > 14)) {
    # Subset the open spaces dataset to the current view, to keep the app responsive
    current_view <- makeRectangle(input$mymap_bounds$north, input$mymap_bounds$east, input$mymap_bounds$south, input$mymap_bounds$west, CRS_ref)
    open_spaces_filtered <- open_spaces[current_view,]
    output$open_spaces_message <- renderUI(HTML(""))
    proxy %>% addPolygons(data = open_spaces_filtered, 
                          popup = paste("open space"), 
                          weight = 2, 
                          col = "green", 
                          opacity = 0.95, 
                          fillOpacity = 0.1,  
                          group = current_group)
  } else {
    proxy %>% clearGroup(current_group)
    if (input$show_open_spaces)
      output$open_spaces_message <- renderUI(HTML("<span style='color:red'><b>(zoom in further)</b></span>"))
    else
      output$open_spaces_message <- renderUI(HTML("(at high zoom only)"))
  }
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_vacant)) {
    proxy %>% clearGroup("vacant")
    return()
  }
  current_group <- "vacant"
  if (input$show_vacant) {
    proxy %>% addPolygons(data = vacant, 
                          popup = paste("vacant and derelict space (2014)"), 
                          weight = 2, 
                          col = "brown", 
                          opacity = 0.95, 
                          fillOpacity = 0.1,  
                          group = current_group)
  } else
    proxy %>% clearGroup(current_group)
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_listed_buildings)) {
    proxy %>% clearGroup("listed_buildings")
    return()
  }
  current_group <- "listed_buildings"
  # Only show listed buildings at high zoom levels, to keep the app responsive
  if (input$show_listed_buildings && (input$mymap_zoom > 16)) {
    # Subset the listed buildings dataset to the current view, to keep the app responsive
    # print(input$mymap_bounds)
    listed_buildings_filtered <- listed_buildings[((listed_buildings$coords.x1 < input$mymap_bounds$east) & (listed_buildings$coords.x1 > input$mymap_bounds$west) & (listed_buildings$coords.x2 > input$mymap_bounds$south) & (listed_buildings$coords.x2 < input$mymap_bounds$north)), ]
    output$listed_buildings_message <- renderUI(HTML(""))
    proxy %>% addCircleMarkers(data = listed_buildings_filtered, 
                               popup = ~paste(sep = "", 
                                              "<b>", ENTITY_REF, "</b>", 
                                              "<br>", "Category ", CATEGORY , " listed building",
                                              "<br>", "<a href='", WEBLINK, "' target='_blank'>Link to Heritage Scotland description</a></b>"), 
                               radius = 5, 
                               col = "black", 
                               opacity = 0.95, 
                               fillOpacity = 0.2,  
                               group = current_group)
  } else {
    proxy %>% clearGroup(current_group)
    if (input$show_listed_buildings)
      output$listed_buildings_message <- renderUI(HTML("<span style='color:red'><b>(zoom in further)</b></span>"))
    else output$listed_buildings_message <- renderUI(HTML("(at high zoom only)"))
  }
})
