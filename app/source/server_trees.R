## server_trees.R: Greener Leith's 1000 trees for Leith project

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

trees <- forest$find()

makeReactiveBinding("login_state")

is_marker_selected <- reactive({
  if(is.null(input$mymap_marker_click))
    return(FALSE) else return(TRUE)
})

selected_marker_group <- reactive({
  if(is.null(input$mymap_marker_click) || is.null(input$mymap_marker_click[[1]]))
    return("none")
  return(str_split(input$mymap_marker_click[[1]], "#")[[1]][1])
})

selected_marker_index <- reactive({
  if(is.null(input$mymap_marker_click) || is.null(input$mymap_marker_click[[1]]))
    return(0)
  return(as.numeric(str_split(input$mymap_marker_click[[1]], "#")[[1]][2]))
})

output$panel_trees <- renderUI({
  if(!(input$show_panel_trees))
    return()
  if (login_state) {
    list(
      checkboxInput("show_my_trees", label = "Show my trees", value = TRUE, width = "100%"),
      checkboxInput("show_other_trees", label = "Show other trees", value = TRUE, width = "100%"),
      radioButtons("tree_edit_mode", label = "Edit mode", choices = c("read only", "add tree", "delete tree"), width = "100%")
    )
  } else {"Please log in first"}
})  

observe({
  
  proxy <- leafletProxy("mymap")
  current_group <- "my_trees"
  
  if((is.null(input$show_my_trees) || !login_state)) {
    proxy %>% clearGroup(current_group)
    return()
  }
  
  if(input$show_my_trees && login_state) {
    if(nrow(trees) > 0) {
      proxy %>% addMarkers(data = filter(trees, user_name == hash_user_name(login_name)), 
                           lat = ~lat,
                           lng = ~lon,
                           popup = ~paste(sep="", "One of my trees (#", id, ")"),
                           icon = red_tree_icon,
                           layerId = ~paste(sep = "#", current_group, id),
                           group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    proxy %>% clearGroup(current_group)
    groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]
  }
  
})

observe({
  
  proxy <- leafletProxy("mymap")
  current_group <- "other_trees"
  
  if(is.null(input$show_other_trees) || !login_state) {
    proxy %>% clearGroup(current_group)
    return()
  }
  
  if(input$show_other_trees && login_state) {
    if(nrow(trees) > 0) {
      proxy %>% addMarkers(data = filter(trees, user_name != hash_user_name(login_name)), 
                           lat = ~lat,
                           lng = ~lon,
                           popup = ~paste(sep="", "Someone else's tree"),
                           icon = green_tree_icon,
                           layerId = ~paste(sep = "#", current_group, id),
                           group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    proxy %>% clearGroup(current_group)
    groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]
  }
})

observeEvent(input$mymap_click, {
  
  if(input$show_panel_trees && login_state) {
    if(input$tree_edit_mode == "add tree") {
      click <- input$mymap_click
      
      if(nrow(trees) > 0)
        new_id <- max(trees$id, na.rm = TRUE) + 1
      else new_id <- 1
      
      trees <<- bind_rows(trees, data.frame(id = new_id, user_name = as.character(hash_user_name(login_name)), lat = click$lat, lon = click$lng, stringsAsFactors = FALSE))
      
      proxy <- leafletProxy("mymap")
      current_group <- "my_trees"
      proxy %>% addMarkers(data = filter(trees, id == new_id), 
                           lat = ~lat,
                           lng = ~lon,
                           popup = ~paste(sep="", "One of my trees (#", id, ")"),
                           icon = red_tree_icon,
                           layerId = ~paste(sep = "#", current_group, id),
                           group = current_group)
      
      forest$insert(data.frame(id = new_id, user_name = as.character(hash_user_name(login_name)), lat = click$lat, lon = click$lng, stringsAsFactors = FALSE))
      
    }
  }
})

observeEvent(input$mymap_marker_click, {
  
  if(input$show_panel_trees && login_state) {
    if(input$tree_edit_mode == "delete tree") {
      if((selected_marker_group() == "my_trees")) {
        
        proxy <- leafletProxy("mymap")
        current_group <- "my_trees"
        proxy %>% removeMarker(paste(sep = "#", current_group, selected_marker_index()))
        
        trees <<- filter(trees, id != selected_marker_index())
        forest$remove(query = paste('{"id" : ', selected_marker_index(), '}', sep = ''))
      }
    } 
  }
})
