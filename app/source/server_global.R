## server_global.R: global functions and constants

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

# Function to create a SpatialPolygons object from bounding box coordinates
makeRectangle <- function (north, east, south, west, CRS_target) {
  SpatialPolygons(list(Polygons(list(Polygon(cbind(c(west,west,east,east,west),c(south,north,north,south,south)))), ID = "current_view")), proj4string = CRS_target)
}

# Function to clean URLs
cleanURL <- function(dirtyURL, link_text) {
  return(as.character(ifelse((is.na(dirtyURL)), "" ,
                             ifelse((dirtyURL == ""), "",
                                    ifelse((str_detect(dirtyURL, "href")), 
                                           paste(sep = "", "<br>", dirtyURL), 
                                           paste(sep = "", "<br><a href='", dirtyURL, "' target='_blank'>", link_text, "</a>"))))))
}

# Function (very basic) to convert plural to singular
plural_to_singular <- function(plural, exceptions) {
  if(plural %in% exceptions)
    return(plural)
  else
    return(plural %>% str_replace("ies$", "y") %>% str_replace("s$", ""))
}

# Function to replace a list of synonyms with the reference term
replace_synonyms <- function(input_string, original, converted) {
  i <- match(input_string, original)
  if(!is.na(i)) return (converted[i])
    else return(input_string)
}


# Function to generate an OSM Overpass query
osm_query <- function(search_term, search_type, search_object) {
  if(search_object == "node")
    query <- paste(sep = "",
                   "[out:xml][timeout:25];
                   area[name='City of Edinburgh']->.searchArea;
                   (
                   node[", search_type, "='", search_term ,"'](area.searchArea);
                   );
                   out;")

  else if(search_object == "way")
    query <- paste(sep = "",
                   "[out:xml][timeout:25];
                   area[name='City of Edinburgh']->.searchArea;
                   (
                   way[", search_type, "='", search_term ,"'](area.searchArea);
                   );
                   out body;
                   >;
                   out skel qt;")

  else if(search_object == "both")
    query <- paste(sep = "",
                   "[out:xml][timeout:25];
                   area[name='City of Edinburgh']->.searchArea;
                   (
                   node[", search_type, "='", search_term ,"'](area.searchArea);
                   way[", search_type, "='", search_term ,"'](area.searchArea);
                   );
                   out body;
                   >;
                   out skel qt;")

  else query <- ""
  return(query)
}

# Function to create icons
news_icon <- makeIcon(iconUrl = "RSS.png")
green_tree_icon <- makeIcon(iconUrl = "green_tree.png", iconAnchorX = 16, iconAnchorY = 32)
red_tree_icon <- makeIcon(iconUrl = "red_tree.png", iconAnchorX = 16, iconAnchorY = 32)

# Some pre-defined categories and palettes

synonyms_original <- c("chemist", "beer_garden", "coffee", "water", "petrol_station", "petrol", "taxi_rank", "cash", "cash_machine", "cash_point", "gp", "gps", "vet", "movy", "club", "nightclub", "graveyard", "market", "churche", "police_station", "postbox", "postboxe", "post_boxe", "kindergarden", "hair salon", "cheesemonger", "fish", "fishmonger", "bike")

synonyms_converted <- c("pharmacy", "biergarten", "cafe", "drinking_water", "fuel", "fuel", "taxi", "atm", "atm", "atm", "doctors", "doctors", "veterinary", "cinema", "night_club", "night_club", "grave_yard", "marketplace", "church", "police", "post_box", "post_box", "post_box", "kindergarten", "hairdresser", "cheese", "seafood", "seafood", "bicycle")

all_project_categories <- list("mooted", "pre-application stage", "application submitted", "approved", "under construction", "completed")

all_osm_names <- c("amenity", "name", "addr.housenumber", "addr.street", "addr.postcode", "opening_hours", "phone", "email", "website",  "twitter", "facebook")

all_recycling_categories <- c("packaging", "paper", "glass", "textile", "compost", "plastic", "book", "can")

project_palette <- colorFactor(palette = "Dark2", projects$status)
dz_2001_palette <- colorFactor(palette = "Blues", boundaries_dz_2001$ZONECODE)
dz_2001_SIMD_palette <- colorNumeric(palette = "RdYlGn", domain = c(1, 6505))
dz_2011_density_scotland_palette <- colorNumeric(palette = "RdYlGn", domain = -c(boundaries_dz_2011$pop_density_min[1], boundaries_dz_2011$pop_density_max[1]))
dz_2011_density_edinburgh_palette <- colorNumeric(palette = "RdYlGn", domain = -boundaries_dz_2011$pop_density)

