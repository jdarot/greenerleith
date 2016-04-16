## projects.R: planning projects

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

output$panel_projects <- renderUI({
  if(!(input$show_panel_projects))
    return()
  list(
    checkboxGroupInput("show_projects", label = "Projects", 
                       choices = list("mooted", "pre-application stage", "application submitted", "approved", "under construction", "completed")
    ),
    checkboxGroupInput("show_audit", label = "Housing land audit", 
                       choices = list("completions", "schedule")
    )
  )
})

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_projects)) {
    proxy %>% clearGroup(all_project_categories)
    return()
  }
  unselected_categories <- all_project_categories[!(all_project_categories %in% input$show_projects)]
  prj <- projects[projects$status %in% input$show_projects, ]
  proxy %>% addCircleMarkers(data = prj,
                             lng = ~lon,
                             lat = ~lat,
                             popup = ~paste(sep = "",
                                            "<b>Project:</b>",
                                            "<br>", "<b>", site, "</b>", " (", status, ")", 
                                            "<br>", "<b>Owner/developer:</b> ", developer, 
                                            "<br>", "<b>Description:</b> ", description,
                                            "<br>", "<b>Latest:</b> ", latest, 
                                            "<br>", "<b>References:</b> ", references,
                                            properURL), 
                             radius = 6,
                             color = ~project_palette(status),
                             opacity = 0.95,
                             fillOpacity = 0.2,
                             group = ~status)
  if (stjames$status %in% input$show_projects) {
    proxy %>% addPolygons(data = stjames,
                          popup = ~paste(sep = "",
                                         "<b>Project:</b>",
                                         "<br>", "<b>", site, "</b>", " (", status, ")", 
                                         "<br>", "<b>Owner/developer:</b> ", developer,
                                         "<br>", "<b>Description:</b> ", description, 
                                         "<br>", "<b>Latest:</b> ", latest, 
                                         "<br>", "<b><a href='", developer_site, "' target='_blank' >Developer's site</a></b> ",
                                         "<br>", "<b><a href='", press, "' target='_blank' >Press articles</a></b> "), 
                          color = ~project_palette(status), 
                          opacity = 0.95, 
                          fillOpacity = 0.2, 
                          group = ~status)
  }
  if (tram_line$status %in% input$show_projects) {
    proxy %>% addPolylines(data = tram_line, 
                           popup = ~paste(sep = "", 
                                          "<b>Project:</b>",
                                          "<br>", "<b>", site, "</b>", " (", status, ")", 
                                          "<br>", "<b>Owner/developer:</b> ", developer,
                                          "<br>", "<b>Description:</b> ", description, 
                                          "<br>", "<b>Latest:</b> ", latest,
                                          "<br>", "<b><a href='", press, "' target='_blank'>Press articles</a></b> "), 
                           color = ~project_palette(status), 
                           opacity = 0.8,
                           group = ~status) %>% 
      addCircles(data = tram_stops, 
                 popup = "proposed tram stop", 
                 color = project_palette(tram_line$status), 
                 opacity = 0.95, 
                 fillOpacity = 0.2, 
                 group = tram_line$status)
  }
  proxy %>% clearGroup(unselected_categories)
})

observe({
  proxy <- leafletProxy("mymap")
  
  current_group <- "completions"
  if (current_group %in% input$show_audit) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = completions, 
                            popup = ~paste(sep = "",
                                           "<b>2014 housing land audit completions:</b> ",
                                           "<br>", ADDRESS1, 
                                           "<br>", Landuse,
                                           "<br>", "<b>Developer:</b> ", DEVELOPER,
                                           "<br>", "<b>Total dwellings:</b> ", TOTAL_DWEL, " (", HOUSES_ALL, " houses, ", FLATS, " flats)",
                                           "<br>", "<b>Of which, affordable:</b> ", AFFORDABLE,
                                           "<br>", "<b>UC_DATE:</b> ", format(as.Date(UC_DATE)), 
                                           "<br>", "<b>Completion date:</b> ", format(as.Date(COMP_DATE)), 
                                           "<br>", "<b>2001-02:</b> ", F2001_02, ", ",
                                           "<b>2002-03:</b> ", F2002_03, 
                                           "<br>", "<b>2003-04:</b> ", F2003_04, ", ",
                                           "<b>2004-05:</b> ", F2004_05, 
                                           "<br>", "<b>2005-06:</b> ", F2005_06, ", ",
                                           "<b>2006-07:</b> ", F2006_07, 
                                           "<br>", "<b>2007-08:</b> ", F2007_08, ", ",
                                           "<b>2008-09:</b> ", F2008_09, 
                                           "<br>", "<b>2009-10:</b> ", F2009_10, ", ",
                                           "<b>2010-11:</b> ", F2010_11, 
                                           "<br>", "<b>2011-12:</b> ", F2011_12, ", ",
                                           "<b>2012-13:</b> ", F2012_13, 
                                           "<br>", "<b>2013-14:</b> ", F2013_14,
                                           "<br>", "<b>NDPS School:</b> ", NDPS_SCHOO, 
                                           "<br>", "<b>RCPS School:</b> ", RCPS_SCHOO
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
  
  current_group <- "schedule"
  if (current_group %in% input$show_audit) {
    if (!(current_group %in% groups_displayed)) {
      proxy %>% addPolygons(data = schedule, 
                            popup = ~paste(sep = "",
                                           "<b>2014 housing land audit schedule:</b> ", 
                                           "<br>", ADDRESS1, 
                                           "<br>", LAND_USE, 
                                           "<br>", "<b>Total dwellings:</b> ", TOTAL_DWEL, " (", HOUSES, 
                                           " houses, ", FLATS, " flats)" , "<br>",
                                           "<b>Of which, affordable:</b> ", AFFORDABLE, 
                                           "<br>", "<b>Complete:</b> ", Complete, ", <b>Remaining:</b> ", Remaining, 
                                           "<br>", "<b>Consent:</b> ", CONSENT_TY, " - ", format(as.Date(CONSENT_DA)) , 
                                           "<br>", "<b>UC_DATE:</b> ", format(as.Date(UC_DATE)), 
                                           "<br>", "<b>2014-15:</b> ", F2014_15, ", ",
                                           "<b>2015-16:</b> ", F2015_16, 
                                           "<br>", "<b>2016-17:</b> ", F2016_17, ", ",
                                           "<b>2017-18:</b> ", F2017_18,
                                           "<br>", "<b>2018-19:</b> ", F2018_19, ", ",
                                           "<b>2014-20:</b> ", F2014___20, 
                                           "<br>", "<b>2019-24:</b> ", F2019_2024, 
                                           "<br>", "<b>NDPS School:</b> ", NDPS_SCHOO, 
                                           "<br>", "<b>RCPS School:</b> ", RCPS_SCHOO
                            ), 
                            weight = 2, 
                            col = "brown", 
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
