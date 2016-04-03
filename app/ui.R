## ui.R: app user interface

## MIT License
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

library(shiny)
library(leaflet)

fluidPage(includeCSS("./www/app.css"),
          
  # Tags for social media
  tags$head(
    tags$meta(property='og:url', content='https://myleith.shinyapps.io/myedinburgh/'),
    tags$meta(property='og:type', content='website'),
    tags$meta(property='og:title', content='Edinburgh Open Data Map'),
    tags$meta(property='og:description', content='Greener Leith open data map of Edinburgh'),
    tags$meta(property='og:image', content='http://greenerleith.org.uk/wp-content/uploads/2013/01/copy-GL_logo_bw-320px.jpg')
  ),  
  
  # We need to create a special div as a workaround to make sure we can specifiy height = 100% (see CSS file)
  div(class="outer", leafletOutput("mymap", height = "100%")),
  
  #######################################################  
  # Authentication - comment out if not enabled
  #######################################################   

  absolutePanel(top = 445, left = 0, draggable = TRUE, class = "controls_autowidth",
   checkboxInput("show_panel_auth", strong("Log in (optional)"), FALSE, width = "100%"),
   uiOutput("panel_auth")
  ),

  ####################################################### 
  
  absolutePanel(top = 0, right = 175, width = 175, draggable = TRUE, class = "controls",  
    checkboxInput("show_panel_projects", strong("Planning projects"), FALSE),    
    uiOutput("panel_projects")            
  ),
          
  absolutePanel(top = 0, right = 0, width = 175, draggable = TRUE, class = "controls",
    checkboxInput("show_panel_planning", strong("Planning framework"), FALSE),            
    uiOutput("panel_planning")
  ),
  
  absolutePanel(top = 0, right = 350, width = 160, draggable = TRUE, class = "controls",  
    checkboxInput("show_panel_air", strong("Air quality"), FALSE),
    uiOutput("panel_air")
  ),
  
  absolutePanel(top = 0, left = 0, draggable = TRUE, id = "panel_air_plot",
    uiOutput("panel_air_plot")
  ),
  
  absolutePanel(top = 0, right = 510, width = 170, draggable = TRUE, class = "controls",  
    checkboxInput("show_panel_gps", strong("GPs, care & schools"), FALSE),    
    uiOutput("panel_gps")            
  ),
  
  absolutePanel(top = 0, left = 0, draggable = TRUE, id = "panel_gps_plot",
    uiOutput("panel_gps_plot")
  ),
  
  absolutePanel(top = 45, right = 0, width = 175, draggable = TRUE, class = "controls",
    checkboxInput("show_panel_stats", strong("Local statistics"), FALSE),
    uiOutput("panel_stats")            
  ),
  
  absolutePanel(top = 45, right = 175, width = 175, draggable = TRUE, class = "controls",
    checkboxInput("show_panel_news", strong("Local news"), FALSE),
    uiOutput("panel_news")            
  ),
  
  absolutePanel(top = 45, right = 350, width = 160, draggable = TRUE, class = "controls",
    checkboxInput("show_panel_amenities", strong("Allotments & recycling"), FALSE),
    uiOutput("panel_amenities")            
  ),
  
  absolutePanel(top = 400, left = 0, draggable = TRUE, class = "controls_autowidth",
    checkboxInput("show_panel_osm", strong("OpenStreetMap data"), FALSE, width = "100%"),
    uiOutput("panel_osm")
  ),
  
  absolutePanel(top = 2, left = 60, width = 140, draggable = TRUE,
    htmlOutput("search_message"),
    textInput("search", label = NULL, width = "120px"),
    actionButton("search_button", strong("Search"), style="padding:6px; font-size:12.5px;width:57px"),
    actionButton("clear_button", strong("Clear"), style="padding:6px; font-size:12.5px;width:57px")
  ),
  
  absolutePanel(top = 110, left = 0, width = 130, draggable = TRUE, id = "background_controls",
    radioButtons("background", label = "Background", choices = list("OpenStreetMap" = 1, "Black & white" = 2, "Light grey" = 3, "Dark" = 4, "Satellite" = 5), selected = 3),
    checkboxInput("retina", strong("Hi-resolution"), FALSE)
  ),
  
  absolutePanel(top = 310, left = 0, draggable = TRUE, class = "controls_autowidth",  
    checkboxInput("show_panel_admin", strong("Boundaries"), FALSE, width = "100%"),            
    uiOutput("panel_admin")            
  ),
          
  absolutePanel(bottom = 0, left = 0, height = 105, draggable = TRUE, id = "credits",
    "Provided by",
    a(img(src = "greener_leith.jpg", height = 50), href = "http://greenerleith.org.uk", target="_blank"),
    HTML("
      <div id='fb-root'></div>
  
      <script type='text/javascript'>(function(d, s, id) {
        var js, fjs = d.getElementsByTagName(s)[0];
        if (d.getElementById(id)) return;
        js = d.createElement(s); js.id = id;
        js.src = '//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.5';
        fjs.parentNode.insertBefore(js, fjs);
      }(document, 'script', 'facebook-jssdk'));
      </script>

      <div class='fb-share-button' data-href='https://myleith.shinyapps.io/myedinburgh' data-layout='button'></div>

      <a href='https://twitter.com/share'
        class='twitter-share-button'
        data-url='http://bit.ly/1nGz4oq'
        data-text='Edinburgh Open Data map'>Tweet
      </a>

      <script type='text/javascript'>!function(d,s,id){
        var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
        if(!d.getElementById(id)){
          js=d.createElement(s);
          js.id=id;js.src=p+'://platform.twitter.com/widgets.js';
          fjs.parentNode.insertBefore(js,fjs);
        }
      }(document, 'script', 'twitter-wjs');
      </script>
             
      <div class='g-plus' data-href='https://myleith.shinyapps.io/myedinburgh' data-height=20 data-annotation='none' data-action='share'></div>
           
      <script src='https://apis.google.com/js/platform.js' async defer></script>
      "),
    
      br(strong(a("Sources, help and contact", href = "https://myleith.wordpress.com/edinburgh-open-data-map/", target="_blank")))
  )
)
