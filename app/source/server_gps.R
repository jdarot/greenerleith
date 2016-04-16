## server_gps.R: GPs, care and schools

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$panel_gps <- renderUI({
  if(!(input$show_panel_gps))
    return()
  list(
    checkboxGroupInput("show_gps", label = "GPs", 
                       choices = list("location" = "GPs", "detailed data" = "show_gps_plot")
    ),    
    checkboxInput("show_dentists", "Dentists", FALSE),
    checkboxInput("show_care_homes", "Care homes", FALSE),
    checkboxInput("show_sheltered_housing", "Sheltered housing", FALSE),
    checkboxGroupInput("show_schools", label = "Schools", 
                       choices = list("nurseries", "primary", "secondary","special")
    ),
    checkboxGroupInput("show_catchment", label = "Catchment areas", 
                       choices = list("primary" = "catch primary", "primary catholic" = "catch primary catholic", "secondary" = "catch secondary", "secondary catholic" = "catch secondary catholic")
    )
  )
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_gps)) {
    proxy %>% clearGroup("GPs")
    return()
  }
  current_group <- "GPs"
  # Note: in loader.R, the GP surgeries are sorted by decreasing size, so that if several surgeries share an identical physical location, the user can still click on them
  if (current_group %in% input$show_gps) {
    proxy %>% addCircles(data = gps,
                         lng = ~lon, 
                         lat = ~lat, 
                         popup = ~paste(sep = "",
                                        "<b>", `Practice Name`, "</b>", 
                                        "<br>", `Address Line 1`,
                                        "<br>", `Address Line 2`, 
                                        "<br>", `Address Line 3`, 
                                        "<br>", `Address Line 4`, 
                                        "<br>", Postcode, 
                                        "<br>", "<b>Tel:</b> ", `Telephone Number`
                         ), 
                         radius = ~sqrt(`All ages`*2), 
                         col = "green", 
                         opacity = 0.9,
                         layerId = paste(sep = "#", rep(current_group, nrow(gps@data)), c(1:nrow(gps@data))),
                         group = current_group)
  } else
    proxy %>% clearGroup(current_group)
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_dentists)) {
    proxy %>% clearGroup("dentists")
    return()
  }
  current_group <- "dentists"
  if (input$show_dentists) {
    proxy %>% addCircleMarkers(data = dentists,
                               lng = ~lon, 
                               lat = ~lat, 
                               popup = ~paste(sep = "", 
                                              "<b>", name, "</b>",
                                              "<br>", address1,
                                              "<br>", address2,
                                              "<br>", address3,
                                              "<br>", postcode,  
                                              "<br>", "<b>Tel:</b> ", phone), 
                               radius = 6, 
                               col = "blue", 
                               opacity = 0.95, 
                               fillOpacity = 0.2, 
                               group = current_group)
  } else
    proxy %>% clearGroup(current_group)
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_care_homes)) {
    proxy %>% clearGroup("care homes")
    return()
  }
  current_group <- "care homes"
  if (input$show_care_homes) {
    proxy %>% addCircleMarkers(data = care_homes,
                               lng = ~lon, 
                               lat = ~lat, 
                               popup = ~paste(sep = "", 
                                              "<b>", Name, "</b>",
                                              "<br>", "<b>Categories:</b> ", Categories,
                                              "<br>", "<b>Specialist care:</b> ", `Specialist care`, 
                                              "<br>", Address, " ", Postcode, 
                                              "<br>", "<b>Tel:</b> ", `Phone number`, 
                                              properURL), 
                               radius = 6, 
                               col = "gray", 
                               opacity = 0.95, 
                               fillOpacity = 0.2, 
                               group = current_group)
  } else
    proxy %>% clearGroup(current_group)
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_sheltered_housing)) {
    proxy %>% clearGroup("sheltered housing")
    return()
  }
  current_group <- "sheltered housing"
  if (input$show_sheltered_housing) {
    proxy %>% addCircleMarkers(data = sheltered_housing,
                               lng = ~lon, 
                               lat = ~lat, 
                               popup = ~paste(sep = "", 
                                              "<b>Sheltered housing:</b> ", "<b>", `Sheltered Housing Complex Name / Address`, "</b>", " - ", Postcode, 
                                              "<br>", "<b>No of units:</b> ", `No of Units`, 
                                              "<br>", "<b>Unit types:</b> ", `Unit Types`, 
                                              "<br>", "<b>Community room:</b> ", `Community Room`, 
                                              "<br>", "<b>Laundry:</b> ", `Laundry`, 
                                              "<br>", "<b>Guest room:</b> ", `Guestroom`, 
                                              "<br>", "<b>Provider:</b> ", Provider,
                                              properURL), 
                               radius = 6, 
                               col = "purple", 
                               opacity = 0.95, 
                               fillOpacity = 0.2, 
                               group = current_group)
  } else
    proxy %>% clearGroup(current_group)
})

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "nurseries"
  if (current_group %in% input$show_schools) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addCircleMarkers(data = nursery,
                                 lng = ~lon, 
                                 lat = ~lat, 
                                 popup = ~paste(sep = "", 
                                                "<b>", Name, "</b>", 
                                                "<br>", Address, 
                                                "<br>", Postcode, 
                                                "<br>", "<b>Tel:</b> ", Telephone, 
                                                "<br>", "<b>Email:</b> ", Email, 
                                                "<br>", "<b>Head teacher:</b> ", Headteacher,
                                                properURL), 
                                 radius = 6, 
                                 col = "purple", 
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
  
  current_group <- "primary"
  if (current_group %in% input$show_schools) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addCircleMarkers(data = primary,
                                 lng = ~lon, 
                                 lat = ~lat, 
                                 popup = ~paste(sep = "", 
                                                "<b>", Name, "</b>", 
                                                "<br>", Address, 
                                                "<br>", Postcode, 
                                                "<br>", "<b>Tel:</b> ", Telephone, 
                                                "<br>", "<b>Email:</b> ", Email, 
                                                "<br>", "<b>Head teacher:</b> ", Head.Teacher, 
                                                "<br>", "<b>Associated secondary school:</b> ", Associated.secondary.school, 
                                                properURL), 
                                 radius = 6, 
                                 col = "blue", 
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
  
  current_group <- "secondary"
  if (current_group %in% input$show_schools) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addCircleMarkers(data = secondary,
                                 lng = ~lon, 
                                 lat = ~lat, 
                                 popup = ~paste(sep = "", 
                                                "<b>", Name, "</b>", 
                                                "<br>", Address, 
                                                "<br>", Postcode, 
                                                "<br>", "<b>Tel:</b> ", Telephone, 
                                                "<br>", "<b>Email:</b> ", Email, 
                                                "<br>", "<b>Head teacher:</b> ", Head.Teacher, 
                                                properURL), 
                                 radius = 6, 
                                 col = "green", 
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
  
  current_group <- "special"
  if (current_group %in% input$show_schools) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addCircleMarkers(data = special,
                                 lng = ~lon, 
                                 lat = ~lat, 
                                 popup = ~paste(sep = "", 
                                                "<b>", Name, "</b>", 
                                                "<br>", Address,
                                                "<br>", Postcode,
                                                "<br>", "<b>Tel:</b> ", Telephone,
                                                "<br>", "<b>Email:</b> ", Email,
                                                "<br>", "<b>Head teacher:</b> ", Head.Teacher,
                                                properURL), 
                                 radius = 6, 
                                 col = "red", 
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
  
  current_group <- "catch primary"
  if (current_group %in% input$show_catchment) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(nondem_primary_areas@data)
      proxy %>% addPolygons(data = nondem_primary_areas,
                            popup = ~paste(sep = "", SCHOOL_NAM, " catchment area"),
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Blues", SCHOOL_NAM)(SCHOOL_NAM), 
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
  
  current_group <- "catch primary catholic"
  if (current_group %in% input$show_catchment) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(cath_primary_areas@data)
      proxy %>% addPolygons(data = cath_primary_areas,
                            popup = ~paste(sep = "", SCHOOL_NAM, " catchment area"),
                            weight = 1.5, 
                            col = "black",
                            fillColor = ~colorFactor(palette = "Blues", SCHOOL_NAM)(SCHOOL_NAM), 
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
  
  current_group <- "catch secondary"
  if (current_group %in% input$show_catchment) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(nondem_secondary_areas@data)
      proxy %>% addPolygons(data = nondem_secondary_areas,
                            popup = ~paste(sep = "", ESTABLIS_2, " catchment area"),
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Greens", ESTABLIS_2)(ESTABLIS_2), 
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
  
  current_group <- "catch secondary catholic"
  if (current_group %in% input$show_catchment) {
    if (!(current_group %in% groups_displayed)) {
      nrow_data <- nrow(cath_secondary_areas@data)
      proxy %>% addPolygons(data = cath_secondary_areas,
                            popup = ~paste(sep = "", SCH_NAME, " catchment area"),
                            weight = 1.5, 
                            col = "black", 
                            fillColor = ~colorFactor(palette = "Greens", SCH_NAME)(SCH_NAME), 
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

selected_gp <- reactive({
  if(is.null(input$show_gps) || !("GPs" %in% input$show_gps) || !(selected_group() == "GPs"))
    return(NULL)
  return(gps[selected_index(),])  
})

gps_plot <- reactive({
  if(is.null(input$show_gps) || !("show_gps_plot" %in% input$show_gps) || !(selected_group() == "GPs") || is.null(selected_gp()))
    return()
  selected_gp()@data %>% select(`0-4`, `5-14`, `15-24`, `25-44`, `45-64`,  `65-74`, `75-84`, `85+`) %>% gather("age_range", "count") %>% ggplot(aes(x = age_range, y = count/sum(count)*100)) + geom_bar(stat = "identity", fill = "dark green") + scale_x_discrete(name = "patient age range") + scale_y_continuous(name = "percentage of total patients") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) -> p
  return(p)
})

output$panel_gps_plot <- renderUI({
  if(is.null(input$show_gps) || !("show_gps_plot" %in% input$show_gps) || !(selected_group() == "GPs") || is.null(selected_gp()))
    return() 
  list(
    HTML(
      "<b>&nbsp", selected_gp()$`Practice Name`, "</b>",
      "<br>", "<b>&nbsp Number of patients: </b>", selected_gp()$`All ages`
    ),
    #          renderValueBox({
    #            
    #            valueBox(
    #              format(selected_gp()$`All ages`, format="d", big.mark=","), "Patients", icon = NULL,
    #              color = "black")
    #          }),
    
    tabBox(width = NULL,
           tabPanel("Patient age distribution",
                    renderPlot(gps_plot(), width = 300, height = 250)    
                    #ggvisOutput("ggvis_age_distribution")
           ),
           tabPanel("GPs",DT::dataTableOutput("gps_table"))
    )
  )
})

output$gps_table <- DT::renderDataTable({
  selected_code <- selected_gp()$`Practice Code`
  gps_doctors %>% filter(`Practice Code` == selected_code) %>% select(`Surname`, Forename, `Middle Initial`, Sex, `GMC Number`) %>% arrange(`Surname`)
  
}, rownames = FALSE, filter = 'none', options = list(
  pageLength = 5, autoWidth = TRUE))
