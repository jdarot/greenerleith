## server_stats.R: local statistics: Census, SIMD etc.

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$panel_stats <- renderUI({
  if(!(input$show_panel_stats))
    return()
  list(
    checkboxInput("show_dz_2011", "Census 2011", FALSE),
    checkboxInput("show_dz_2001", "SIMD 2012", FALSE),
    radioButtons("SIMD", label = NULL,
                 choices = list("Overall" = "overall", "Employment" = "employment", "Income" = "income","Health" = "health", "Education" = "education", "Access to services" = "access_to_services", "Drive times" = "drive_times", "Public transport" = "public_transport", "Crime" = "crime", "Housing" = "housing", "House prices (2013)" = "housing_prices"), selected = "overall"
    )
  )
}) 

boundaries_dz_2001_colour <- reactive({
  if (input$SIMD == "overall") return(boundaries_dz_2001$SIMD_2012)
  if (input$SIMD == "employment") return(boundaries_dz_2001$SIMD_2012_employment)
  if (input$SIMD == "income") return(boundaries_dz_2001$SIMD_2012_income)
  if (input$SIMD == "health") return(boundaries_dz_2001$SIMD_2012_health)
  if (input$SIMD == "education") return(boundaries_dz_2001$SIMD_2012_education)
  if (input$SIMD == "access_to_services") return(boundaries_dz_2001$SIMD_2012_access_to_services)
  if (input$SIMD == "drive_times") return(boundaries_dz_2001$SIMD_2012_drive_times)
  if (input$SIMD == "public_transport") return(boundaries_dz_2001$SIMD_2012_public_transport)
  if (input$SIMD == "crime") return(boundaries_dz_2001$SIMD_2012_crime)
  if (input$SIMD == "housing") return(boundaries_dz_2001$SIMD_2012_housing)
  if (input$SIMD == "housing_prices") return((1+6505-boundaries_dz_2001$housing_prices_2013_rank))
  return(NULL)
})

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "dz_2001"
  
  if(is.null(input$show_dz_2001)) {
    proxy %>% clearGroup(current_group)
    return()
  }
  
  if (input$show_dz_2001) {
    
    nrow_data <- nrow(boundaries_dz_2001@data)
    dz_2001_palette <- dz_2001_SIMD_palette
    
    proxy %>% addPolygons(data = boundaries_dz_2001, 
                          popup = ~paste(sep = "", 
                                         "<b>Scottish Index of Multiple Deprivation</b>",
                                         "<br>", "Data zone: ", ZONECODE, "/2001",
                                         "<br>", "Rank from 6505 data zones in Scotland", 
                                         "<br>", "Smaller number = more deprived",
                                         "<br>", "Data: 2012",
                                         "<br>",
                                         "<br>", "<b><a href = 'http://simd.scotland.gov.uk/publication-2012/introduction-to-simd-2012/overview-of-the-simd/what-is-the-simd/' target='_blank'>Overall:</a> ", SIMD_2012, "</b>",
                                         "<br>", "<a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/employment-domain/' target='_blank'>Employment:</a> ", SIMD_2012_employment,
                                         "<br>", "<a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/income-domain/' target='_blank'>Income:</a> ", SIMD_2012_income,
                                         "<br>", "<a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/health-domain/' target='_blank'>Health:</a> ", SIMD_2012_health,
                                         "<br>", "<a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/education-skills-and-training-domain/' target='_blank'>Education:</a> ", SIMD_2012_education,
                                         "<br>", "<a href =http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/geographic-access-domain/' target='_blank'>Access to services:</a> ", SIMD_2012_access_to_services,
                                         "<br>", "&nbsp&nbspof which, <a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/geographic-access-domain/' target='_blank'>Drive times:</a> ", SIMD_2012_drive_times,
                                         "<br>", "&nbsp&nbspof which, <a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/geographic-access-domain/' target='_blank'>Public transport:</a> ", SIMD_2012_public_transport,
                                         "<br>", "<a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/crime-domain/' target='_blank'>Crime:</a> ", SIMD_2012_crime,
                                         "<br>", "<a href = 'http://simd.scotland.gov.uk/publication-2012/technical-notes/domains-and-indicators/housing-domain/' target='_blank'>Housing:</a> ", SIMD_2012_housing,
                                         "<br>",
                                         "<br>", "<b>Median house sale price (2013):</b> £", formatC(housing_prices_2013_median, format = "d", big.mark=','), 
                                         "<br>", "(", housing_prices_2013_rank, ifelse(housing_prices_2013_rank == 1, "st", ifelse(housing_prices_2013_rank == 2, "nd", ifelse(housing_prices_2013_rank == 3, "rd",  "th"))) , " most expensive place in Scotland)"      
                          ), 
                          weight = 1.5, 
                          col = "black", 
                          fillColor = dz_2001_palette(boundaries_dz_2001_colour()), 
                          opacity = 0.9, 
                          fillOpacity = 0.4, 
                          layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                          group = current_group)
  } else
    proxy %>% clearGroup(current_group)
  
})    

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "dz_2011"
  
  if(is.null(input$show_dz_2011)) {
    proxy %>% clearGroup(current_group)
    return()
  }
  
  if (input$show_dz_2011) {
    
    nrow_data <- nrow(boundaries_dz_2011@data)
    proxy %>% addPolygons(data = boundaries_dz_2011, 
                          popup = ~paste(sep = "", 
                                         "<b>", Name, "</b>",
                                         "<br>", "Data zone: ", DataZone, "/2011",
                                         "<br>", "<b>Population (2011): </b>", formatC(TotPop2011, format = "d", big.mark=','), 
                                         "<br>", "<b>Households (2011): </b>", formatC(HHCnt2011, format = "d", big.mark=','),
                                         "<br>", "<b>Population density: </b>", formatC(round(pop_density), format = "d", big.mark=','), "/km2",
                                         "<br>", "(", pop_density_rank, ifelse(pop_density_rank == 1, "st", ifelse(pop_density_rank == 2, "nd", ifelse(pop_density_rank == 3, "rd",  "th"))), " most densely populated data zone",
                                         "<br>", " in Scotland, out of 6976. Mean density ", round(pop_density_mean), "/km2)"
                          ), 
                          weight = 1.5, 
                          col = "black", 
                          fillColor = dz_2011_density_edinburgh_palette(-boundaries_dz_2011$pop_density), 
                          opacity = 0.9, 
                          fillOpacity = 0.4, 
                          layerId = paste(sep = "#", rep(current_group, nrow_data), c(1:nrow_data)),
                          group = current_group)
    
  } else 
    proxy %>% clearGroup(current_group)
})
