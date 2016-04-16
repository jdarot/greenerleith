# server_map.R: map including initialisation, background and highlighting

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

# Initialise the map

output$mymap <- renderLeaflet({
  leaflet() %>% setView(lng = -3.178, lat = 55.965, zoom = 13)
})

# Create a vector to keep track of displayed groups
# this is necessary in order to not redraw layers unnecessarily
# as unfortunately I cannot see a way to test whether a group is empty in a map in the leaflet package

groups_displayed <- c()

# Add tiles

observe({
  proxy <- leafletProxy("mymap")
  
  # Below (and everywhere), we could just hide/show each group using hideGroup() / showGroup() instead of clearGroup(), but it is almost as quick to delete and recreate it as needed, and it keeps the map size small
  
  if (input$retina == FALSE) {
    if (input$background == 1) {
      proxy %>% clearGroup(c("CartoDB", "Toner", "Toner Lite", "Satellite")) %>% addTiles(group = "OSM", attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>")
    }
    if (input$background == 2) {
      proxy %>% clearGroup(c("CartoDB", "OSM", "Toner Lite", "Satellite")) %>% addProviderTiles("Stamen.Toner", group = "Toner", options = providerTileOptions(attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a> &copy <a href = 'http://stamen.com/' target='_blank'>Stamen Design</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>"))
    }
    if (input$background == 3) {
      proxy %>% clearGroup(c("CartoDB", "OSM", "Toner", "Satellite")) %>% addProviderTiles("Stamen.TonerLite", group = "Toner Lite", options = providerTileOptions(attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a> &copy <a href = 'http://stamen.com/' target='_blank'>Stamen Design</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>"))
    }
    if (input$background == 4) {
      proxy %>% clearGroup(c("OSM", "Toner", "Toner Lite", "Satellite")) %>% addProviderTiles("CartoDB.DarkMatter", group = "CartoDB", options = providerTileOptions(attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a> &copy <a href = 'https://cartodb.com/attributions' target='_blank'>CartoDB</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>"))
    }
    if (input$background == 5) {
      proxy %>% clearGroup(c( "CartoDB", "OSM", "Toner", "Toner Lite")) %>% addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite", attribution = "&copy; <a href = 'http://www.esri.com/' target='_blank'>Esri</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>")
    }     
  } else {
    if (input$background == 1) {
      proxy %>% clearGroup(c("CartoDB", "Toner", "Toner Lite", "Satellite")) %>% addTiles(group = "OSM", attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>", options = tileOptions(detectRetina = TRUE))
    }
    if (input$background == 2) {
      proxy %>% clearGroup(c("CartoDB", "OSM", "Toner Lite", "Satellite")) %>% addTiles("http://tile.stamen.com/toner/{z}/{x}/{y}@2x.png", group = "Toner", attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a> &copy <a href = 'http://stamen.com/' target='_blank'>Stamen Design</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>")
    }
    if (input$background == 3) {
      proxy %>% clearGroup(c("CartoDB", "OSM", "Toner", "Satellite")) %>% addTiles("http://tile.stamen.com/toner-lite/{z}/{x}/{y}@2x.png", group = "Toner Lite", attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a> &copy <a href = 'http://stamen.com/' target='_blank'>Stamen Design</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>")
    }
    if (input$background == 4) {
      proxy %>% clearGroup(c("OSM", "Toner", "Toner Lite", "Satellite")) %>% addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}@2x.png", group = "CartoDB", options = providerTileOptions(attribution = "&copy <a href = 'http://www.openstreetmap.org' target='_blank'>OpenStreetMap</a> &copy <a href = 'https://cartodb.com/attributions' target='_blank'>CartoDB</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>"))
    }
    if (input$background == 5) {
      proxy %>% clearGroup(c( "CartoDB", "OSM", "Toner", "Toner Lite")) %>% addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite", attribution = "&copy; <a href = 'http://www.esri.com/' target='_blank'>Esri</a>, <a href = 'https://myleith.wordpress.com/edinburgh-open-data-map/' target='_blank'>Public sector and Ordnance Survey data attribution</a>", options = tileOptions(detectRetina = TRUE))
    }            
  }
})

# Highlight selected shapes

is_selected <- reactive({
  if(is.null(input$mymap_shape_click))
    return(FALSE) else return(TRUE)
})

selected_group <- reactive({
  if(is.null(input$mymap_shape_click) || is.null(input$mymap_shape_click[[1]]))
    return("none")
  return(str_split(input$mymap_shape_click[[1]], "#")[[1]][1])
})

selected_index <- reactive({
  if(is.null(input$mymap_shape_click) || is.null(input$mymap_shape_click[[1]]))
    return(0)
  return(as.numeric(str_split(input$mymap_shape_click[[1]], "#")[[1]][2]))
})

observe({
  proxy <- leafletProxy("mymap")
  proxy %>% clearGroup("highlight")
  highlighted_data <- NULL
  if(is_selected()) {
    if(input$show_panel_gps) {
      if((selected_group() == "catch primary")&&(selected_group() %in% input$show_catchment)) 
        highlighted_data <- nondem_primary_areas[selected_index(),]
      if((selected_group() == "catch secondary")&&(selected_group() %in% input$show_catchment))  
        highlighted_data <- nondem_secondary_areas[selected_index(),]
      if((selected_group() == "catch primary catholic")&&(selected_group() %in% input$show_catchment))  
        highlighted_data <- cath_primary_areas[selected_index(),]
      if((selected_group() == "catch secondary catholic")&&(selected_group() %in% input$show_catchment))  
        highlighted_data <- cath_secondary_areas[selected_index(),]
    }
    if(input$show_panel_admin) {
      if((selected_group() == "pcs")&&(selected_group() %in% input$show_admin))  
        highlighted_data <- boundaries_pcs[selected_index(),]
      if((selected_group() == "unitary")&&(selected_group() %in% input$show_admin))  
        highlighted_data <- boundaries_unitary[selected_index(),]
      if((selected_group() == "const")&&(selected_group() %in% input$show_admin))  
        highlighted_data <- boundaries_const[selected_index(),]
      if((selected_group() == "ward")&&(selected_group() %in% input$show_admin)) 
        highlighted_data <- boundaries_ward[selected_index(),]
      if((selected_group() == "west")&&(selected_group() %in% input$show_admin)) 
        highlighted_data <- boundaries_west[selected_index(),]
      if((selected_group() == "ccs")&&(selected_group() %in% input$show_admin))  
        highlighted_data <- boundaries_cc[selected_index(),]
      if((selected_group() == "nns")&&(selected_group() %in% input$show_admin))  
        highlighted_data <- boundaries_nn[selected_index(),]
      if((selected_group() == "nps")&&(selected_group() %in% input$show_admin))  
        highlighted_data <- boundaries_np[selected_index(),]
    }
    if(input$show_panel_stats) {
      if((selected_group() == "dz_2011")&&(selected_group() %in% input$show_stats))  
        highlighted_data <- boundaries_dz_2011[selected_index(),]
      if((selected_group() == "dz_2001")&&(selected_group() %in% input$show_stats))  
        highlighted_data <- boundaries_dz_2001[selected_index(),]
    }
    if(!is.null(highlighted_data))
      proxy %>% addPolygons(
        data = highlighted_data,
        weight = 2.5, 
        col = "red", 
        fillColor = "white", 
        opacity = 1, 
        fillOpacity = 0,
        group = "highlight")
  }
})
