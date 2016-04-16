## server_admin.R:  administrative boundaries (and associated data where available)

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$panel_admin <- renderUI({
  if(!(input$show_panel_admin))
    return()
  list(
    checkboxGroupInput("show_admin", label = NULL, width = "100%",
                       choices = list("Councils" = "unitary", "MP constituencies" = "west", "MSP constituencies" = "const", "Wards" = "ward", "Postcode sectors" = "pcs", "Community councils" = "ccs", "Natural neighbourhoods" = "nns", "Neighbourhood partnerships" = "nps")
    )
  )
})  

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "pcs"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_pcs@data)
      proxy %>% addPolygons(data = boundaries_pcs, 
                            popup = ~paste(sep = "", NAME), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NAME)(NAME), 
                            opacity = 0.9, 
                            fillOpacity = 0.4,
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "unitary"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_unitary@data)
      proxy %>% addPolygons(data = boundaries_unitary, 
                            popup = ~paste(sep = "", NAME), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NAME)(NAME), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "const"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_const@data)
      proxy %>% addPolygons(data = boundaries_const, 
                            popup = ~paste(sep = "", NAME), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NAME)(NAME), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "ward"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_ward@data)
      proxy %>% addPolygons(data = boundaries_ward, 
                            popup = ~paste(sep = "",
                                           "<b>", NAME, "</b>",
                                           "<br>", "<b>Population (2013): </b>", formatC(pop_2013, format = "d", big.mark=','), 
                                           "<br>", "<b>Population density: </b>", formatC(round(pop_density), format = "d", big.mark=','), "/km2",
                                           "<br>", "(", pop_density_rank, ifelse(pop_density_rank == 1, "st", ifelse(pop_density_rank == 2, "nd", ifelse(pop_density_rank == 3, "rd",  "th"))), 
                                           " most densely populated",
                                           "<br>", "ward in Scotland, out of 353)", "<b>", URL, "</b>"
                            ), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NAME)(NAME), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "west"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_west@data)
      proxy %>% addPolygons(data = boundaries_west, 
                            popup = ~paste(sep = "", NAME), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NAME)(NAME), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "ccs"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_cc@data)
      proxy %>% addPolygons(data = boundaries_cc,
                            popup = ~paste(sep = "", 
                                           "<b>", LABEL, " Community Council", "</b>",
                                           properURL,
                                           properWebsite,
                                           properFacebook,
                                           properTwitter), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", LABEL)(LABEL), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "nns"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_nn@data)
      proxy %>% addPolygons(data = boundaries_nn, 
                            popup = ~paste(sep = "", NATURALCOM), 
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NATURALCOM)(NATURALCOM), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
  
  current_group <- "nps"
  if (current_group %in% input$show_admin) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(boundaries_np@data)
      proxy %>% addPolygons(data = boundaries_np, 
                            popup = ~paste(sep = "", 
                                           NP_Name, "<br>",
                                           "<b><a href='", Link, "' target='_blank'>Link to website</a></b> "),
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", NP_Name)(NP_Name), 
                            opacity = 0.9, 
                            fillOpacity = 0.4, 
                            layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                            group = current_group)
      groups_displayed <<- c(groups_displayed, current_group)
    }
  } else {
    if ((current_group %in% groups_displayed)) {
      proxy %>% clearGroup(current_group)
      groups_displayed <<- groups_displayed[-which(groups_displayed==current_group)]}
  }
})
