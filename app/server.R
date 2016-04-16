## server.R: app server-side scripting

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

library(shiny)
library(leaflet)
library(sp)
library(rgeos)
library(ggmap)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(dplyr)
library(openair)
library(DT)
library(shinydashboard)
library(xml2)
library(overpass)

load(file = "./data/payload.RData")

# Global functions and constants
source("./source/server_global.R", local = TRUE)

authentication_enabled <- TRUE

# Authentication setup
if (authentication_enabled) {
  library(mongolite)
  library(bcrypt)
  #library(sodium)
  library(digest)
  source("./private/server_authentication_setup.R", local = TRUE)
}

shinyServer(function(input, output, session) {

  # Map including initialisation, background and highlighting
  source("./source/server_map.R", local = TRUE) 

  # Authentication
  if (authentication_enabled)
    source("./source/server_authentication.R", local = TRUE)
    
  # Address search
  source("./source/server_search.R", local = TRUE)
  
  # Air quality data
  source("./source/server_air.R", local = TRUE) 

  # News stories
  source("./source/server_news.R", local = TRUE) 
  
  # OpenStreetMap data
  source("./source/server_osm.R", local = TRUE) 

  # Planning projects
  source("./source/server_projects.R", local = TRUE) 
  
  # Planning framework
  source("./source/server_planning.R", local = TRUE) 
  
  # GPs, care and schools
  source("./source/server_gps.R", local = TRUE) 
  
  # Local statistics: Census, SIMD etc.
  source("./source/server_stats.R", local = TRUE) 
  
  # Amenities: allotments, recycling
  source("./source/server_amenities.R", local = TRUE) 
    
  # Administrative boundaries (and associated data where available)
  source("./source/server_admin.R", local = TRUE) 
  
  # Green Leith thousand trees for Leith project
  source("./source/server_trees.R", local = TRUE) 
  
})