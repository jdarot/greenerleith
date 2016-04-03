## loader.R: load and process data for use in the app

## MIT License
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

library(magrittr)
library(tidyr)
library(plyr)
library(dplyr)  # needs to be loaded AFTER plyr, else dplyr's summarise() function will be masked by plyr's function of the same name and cause issues
library(ggmap)
library(leaflet)
library(readr)
library(readxl)
library(rgdal)
library(rgeos)
library(openair)
library(stringr)
#devtools::install_github('hrbrmstr/overpass')
library(overpass)

# Clear environment
rm(list = ls())

# Use a reference CRS in order to enable direct plotting in Leaflet
# setwd("C:/Jeremy/GL/R/Spatial/Creating-maps-in-R-master")
# lnd84 <- readRDS(file = "data/lnd84.Rds")
# CRS_ref <- CRS(proj4string(lnd84))
# rm(lnd84)

# Switch to working directory
setwd("C:/Jeremy/GL/R/")
# On VDL:
#setwd("//tsclient/H/GL/R/")

# Source the cleanURL function
source("./app/helpers.R")

# Local Development Plan (LDP) boundary mask (CEC). Used to define the boundary box to be used later for address searches in the app
ldp <- readOGR(dsn = "../data/Council/LDP", layer = "LDP Boundary") %>% spTransform(CRSobj = CRS_ref)
min_lon <- ldp@bbox[1, 1]
max_lon <- ldp@bbox[1, 2]
min_lat <- ldp@bbox[2, 1]
max_lat <- ldp@bbox[2, 2]

# Planning projects: sites of interest to the LCCC. Source: Google docs at http://leithcentralcc.co.uk/planning/ (with some additional clean-up)
projects <- read_excel("../data/LCCC/LCCC_projects.xls")
projects %<>% rename(latest = `latest status`, developer = `developer/owner`, link = `link to Council planning portal`)
projects$status <- factor(projects$status, levels = c("mooted", "pre-application stage", "application submitted", "approved", "under construction", "completed"))
# Convert address to coordinates using ggmap::geocode. (The reverse operation can be done using revgeocode())
projects %<>% mutate(lon = geocode(projects$address, source = "google")$lon, lat = geocode(projects$address, source = "google")$lat, properURL = cleanURL(link, "Link to Council website"))
View(projects)

# Other planning projects (custom)

# Read shapefiles of tram route safeguard and proposed stops from draft LDP data (CEC, under licence as it is OS-derived, not open data)
tram_line <- readOGR(dsn = "../data/Council/LDP", layer = "Tram Route Safeguard")
tram_stops <- readOGR(dsn = "../data/Council/LDP", layer = "Proposed Tram Stop")
# Clip the tram safeguard data to the area currently proposed for extension (from York Place to McDonald Road, Foot of the Walk, Ocean Terminal or Newhaven)
# Read boundary polygon (drawn manually and saved as a geojson file on geojson.io), reproject to enable clipping
tram_limits <- readOGR("../data/Custom/tramlimits.geojson", "OGRGeoJSON") %>% spTransform(tram_limits, CRSobj = CRS(proj4string(tram_line)))
# To clip a line, we need to use rgeos::gIntersection
tram_line <- gIntersection(tram_line, tram_limits)
# For points, we can just use the sp::over (with the shortcut notation below)
tram_line %<>% spTransform(CRSobj = CRS_ref)
tram_stops <- tram_stops[tram_limits, ]
tram_stops %<>% spTransform(CRSobj = CRS_ref)
# Test that the clipping worked
plot(tram_limits, col = "red")
plot(tram_line, add = TRUE)
plot(tram_stops, add = TRUE)
rm(tram_limits)

# Read polygon for the St James Centre redevelopment (drawn manually and saved as a geojson file on geojson.io)
stjames <- readOGR("../data/Custom/stjames.geojson", "OGRGeoJSON") %>% spTransform(CRSobj = CRS_ref)

# Add non-spatial data for other projects (tram and St James Centre)
other_projects <- read_excel("../data/Custom/Other_projects.xls")
tram_project <- other_projects[other_projects$site == "Tram extension", ]
tram_line <- SpatialLinesDataFrame(tram_line, data = data.frame(tram_project))
stjames_project <- other_projects[other_projects$site == "St James Centre", ]
stjames@data <- cbind(stjames@data, stjames_project)
rm(stjames_project)
rm(tram_project)

# 2014 land audit completions (CEC)
completions <- readOGR("../data/Council/Housing land audit", layer = "Housing_land_audit_completions") %>% spTransform(CRSobj = CRS_ref)

# 2014 land audit schedule (CEC)
schedule <- readOGR("../data/Council/Housing land audit", layer = "Housing_land_audit_schedule") %>% spTransform(CRSobj = CRS_ref)
# I get an error - dropping null geometries: 434

# LDP housing proposals (CEC)
housing_prop <- readOGR(dsn = "../data/Council/LDP", layer = "Housing Proposal (HSG 1 - HSG 37)") %>% spTransform(CRSobj = CRS_ref)

# LDP green space proposals (CEC)
green_prop <- readOGR(dsn = "../data/Council/LDP", layer = "Greenspace Proposal (GS1-11)") %>% spTransform(CRSobj = CRS_ref)

# LDP indicative school proposals (CEC)
school_prop <- readOGR(dsn = "../data/Council/LDP", layer = "Indicative School Proposal (SCH 1-3, SCH 6-8)") %>% spTransform(CRSobj = CRS_ref)

# World Heritage Site (CEC)
world_heritage <- readOGR(dsn = "../data/Council/LDP", layer = "World Heritage Site") %>% spTransform(CRSobj = CRS_ref)

# Designated Conservation Areas (CEC)
conservation_areas <- readOGR(dsn = "../data/Council/Conservation Areas", layer = "Conservation_areas") %>% spTransform(CRSobj = CRS_ref)
conservation_areas$properURL = cleanURL(conservation_areas$Link, "Link to Council website")

# Green belt (CEC)
green_belt <- readOGR(dsn = "../data/Council/LDP", layer = "Green Belt") %>% spTransform(CRSobj = CRS_ref)

# Tree preservation orders (CEC)
tree_orders <- readOGR(dsn = "../data/Council/Tree Preservation Orders", layer = "tpo") %>% spTransform(CRSobj = CRS_ref)

# Living Landscapes (CEC)
living_landscapes <- readOGR(dsn = "../data/Council/livinglandscapes", layer = "LivingLandscapes") %>% spTransform(CRSobj = CRS_ref)

# Open spaces (CEC)
open_spaces <- readOGR(dsn = "../data/Council/LDP", layer = "Open Space") %>% spTransform(CRSobj = CRS_ref)
# there is a warning message when loading

# Allotments (CEC)
allotments <- read_csv("../data/Council/Allotments/allotments.csv") %>% separate(Location, c("lat", "lon"), sep = ",") %>% separate(`Waiting time`, "wait", sep = " ", extra = "drop", remove = FALSE) %>% separate(`Total plots`, "plots", sep = " ", extra = "drop", remove = FALSE) %>% mutate(wait = as.numeric(wait), plots = as.numeric(plots), lat = as.numeric(lat), lon = as.numeric(lon))

# Recycling (CEC)
recycling <- read_csv("../data/Council/Recycling/recyclingpoints.csv") %>% mutate(lat = Latitude, lon = Longitude, type = BankTypeNa, site = Site_Name, location = RecyclingS) %>% select(site, location, type, lat, lon) %>% mutate(lat = as.numeric(lat), lon = as.numeric(lon))
recycling$cat <- ifelse(str_sub(recycling$type, 1, 6) == "Bottle", "glass", ifelse(str_sub(recycling$type, 1, 9) == "Packaging", "packaging", ifelse(str_sub(recycling$type, 1, 5) == "Paper", "paper", ifelse(str_sub(recycling$type, 1, 7) == "Textile", "textile", ifelse(str_sub(recycling$type, 1, 4) == "Book", "book", ifelse(str_sub(recycling$type, 1, 3) == "Can", "can", ifelse(str_sub(recycling$type, 1, 7) == "Plastic", "plastic", ifelse(str_sub(recycling$type, 1, 7) == "Compost", "compost", NA ))))))))
sum(is.na(recycling$cat))
#levels(factor(recycling$type))
summary(factor(recycling$type))
all_recycling_categories <- c("packaging", "paper", "glass", "textile", "compost", "plastic", "book", "can")
recycling$n <- match(recycling$cat, all_recycling_categories)
rm(all_recycling_categories)
recycling %<>% arrange(desc(n))
recycling_colours <- c("green", "blue", "purple", "pink", "brown", "red", "black", "grey")
recycling$colour <- recycling_colours[recycling$n]
rm(recycling_colours)

# Vacant and Derelicy Land Survey 2014 (CEC)
vacant <- readOGR(dsn = "../data/Council/vacant_and_derelict_land_survey", layer = "SDVLS_2014") %>% spTransform(CRSobj = CRS_ref)

# Listed buildings (the original data from Historic Scotland is Scotland-wide and large, so we need to clip it early on)
listed_buildings <- readOGR(dsn = "../data/Historic Scotland", layer = "Listed_Buildings") %>% spTransform(CRSobj = CRS_ref)
listed_buildings <- listed_buildings[ldp, ]
plot(listed_buildings)
plot(ldp, add = TRUE, col = "red")
listed_buildings$ENTITY_REF %<>% as.character() %>% str_to_title()

# Air quality data: BUSH, ED, ED3, GRA2, GRAN air pollution data (using openair package)
# Location of the station (copy/pasted from the Scotland Air Quality website) (the air quality data is loaded dynamically in the app itself)
air_stations <- read_excel("../data/Custom/air_stations.xls")

# GP data from ISD (Scotland-wide)

# GP contact details
gps_contact <- read_excel("../data/ISD/Prac_ContactDetails_Oct2015_final.xls", sheet = "GP practice details 01 Oct2015", skip = 5)
gps_contact %<>% mutate(`Practice Code` = as.character(`Practice Code`))
gps_doctors <- read_excel("../data/ISD/GP_ContactDetails_Oct2015_final.xls", sheet = "GP contact details - 01 Oct2015", skip = 5)
gps_doctors %<>% filter(`NHS Board Name` == "LOTHIAN") %>% select(`Practice Code`, `GMC Number`, `Forename`, `Middle Initial`, `Surname`, `Sex`, `Dispensing`) %>% mutate(`Practice Code` = as.character(`Practice Code`), `GMC Number` = as.character(`GMC Number`)) %>% mutate(Sex = revalue(Sex, c("M" = "Male", "F" = "Female")), Dispensing = revalue(Dispensing, c("Y" = "Yes", "N" = "No")))

# GP practice size data (quarterly for all patients (male and female))
gps_all <- read_excel("../data/ISD/Prac_ListSize_Oct2015_final.xls", sheet = "All", skip = 6)
gps_all %<>% rename(`Practice Code` = `Practice code`)
gps_all %<>% filter(!is.na(`Practice Code`)) %>% select(-`NHS Board`)
gps <- left_join(gps_contact, gps_all)
gps %<>% filter(`NHS Board Name` == "LOTHIAN")
google_geocoding <- geocode(paste(gps$`Address Line 2`, gps$`Address Line 3`, gps$`Address Line 4`, gps$Postcode, sep = " "), source = "google")
gps %<>% mutate(lon = google_geocoding$lon, lat = google_geocoding$lat)

# Save the data, as the geocoding takes a while
saveRDS(object = gps, file = "../data/Custom/gps.Rds")
gps <- readRDS(file = "../data/Custom/gps.Rds")

# Need to fix failure of geocoding for one surgery
gps[gps$`Practice Code` == "70959", ]$lon <- geocode(gps[gps$`Practice Code` == "70959", ]$Postcode, source = "google")$lon
gps[gps$`Practice Code` == "70959", ]$lat <- geocode(gps[gps$`Practice Code` == "70959", ]$Postcode, source = "google")$lat
gps %<>% filter(!is.na(lon))

# Note: below, the GP surgeries are sorted by decreasing size, so that if several surgeries share an identical physical location, the user can still click on them
gps %<>% rename(`All ages` = `All Ages`, `5-14` = `05-14`)
gps %<>% arrange(desc(`All ages`))
gps_spatial <- SpatialPointsDataFrame(coords = cbind(gps$lon, gps$lat), data = as.data.frame(gps), proj4string = CRS_ref)
local_gps <- gps_spatial[ldp, ]
plot(local_gps)
gps <- local_gps
gps$`0-4` %<>% as.numeric()
gps$`5-14` %<>% as.numeric()
gps$`15-24` %<>% as.numeric()
gps$`25-44` %<>% as.numeric()
gps$`45-64` %<>% as.numeric()
gps$`65-74` %<>% as.numeric()
gps$`75-84` %<>% as.numeric()
gps$`85+` %<>% as.numeric()
rm(list = c("gps_contact", "gps_all", "local_gps", "google_geocoding", "gps_spatial"))

# Dentists (CEC)
dentists <- read_csv("../data/Council/Dentists/dentistsbypostcode.csv")
dentists %<>%
  mutate(name = paste(Description, Forename, Surname)) %>%
  rename(id = `Loc Location No`, address1 = `Address Line 1`, address2 = `Address Line 2`, address3 = `Address Line 3`, postcode = `Postcode nice`, phone = `Phone Number`) %>%
  select(id, name, address1, address2, address3, postcode, phone) %>%
  group_by(id) %>%
  summarise(name = paste(name, collapse = ", "), address1 = first(address1), address2 = first(address2), address3 = first(address3), postcode = first(postcode), phone = first(phone)) %>%
  filter(!is.na(id))
google_geocoding <- geocode(paste(dentists$postcode), source = "google")
dentists %<>% mutate(lon = google_geocoding$lon, lat = google_geocoding$lat)
dentists_spatial <- SpatialPointsDataFrame(coords = cbind(dentists$lon, dentists$lat), data = as.data.frame(dentists), proj4string = CRS_ref)
local_dentists <- dentists_spatial[ldp, ]
plot(local_dentists)
dentists <- local_dentists
rm(list = c("local_dentists", "google_geocoding", "dentists_spatial"))

# Care homes (CEC)
care_homes <- read_csv("../data/Council/Care Homes/carehomesresidentialandnursing.csv") %>% separate(Location, c("lat", "lon"), sep = ",")
care_homes %<>% mutate(properURL = cleanURL(Website, "Website"))

# Sheltered housing (CEC)
sheltered_housing <- read_csv("../data/Council/Sheltered Housing/shelteredhousingcomplexes.csv") %>% separate(Location, c("lat", "lon"), sep = ",")
sheltered_housing  %<>% mutate(properURL = cleanURL(`Web Page Link`, "Website"))

# School locations (CEC)

nursery <- read.csv("../data/Council/Schools/Locations/nurseryschoolsclassesandearlyyearscentres.csv")
nursery %<>% separate(Location, c("lat", "lon"), sep = ",")
# some nurseries don't have a location, require a reverse geocoding
nursery[nursery$lon == "", ]$lon <- geocode(as.character(nursery[nursery$lon == "", ]$Postcode), source = "google")$lon
nursery[nursery$lat == "", ]$lat <- geocode(as.character(nursery[nursery$lat == "", ]$Postcode), source = "google")$lat
nursery %<>% mutate(properURL = cleanURL(Establishment.website, "Nursery website"))

primary <- read.csv("../data/Council/Schools/Locations/primaryschools.csv")
primary %<>% separate(Location, c("lat", "lon"), sep = ",")
primary %<>% mutate(properURL = cleanURL(More.information, "School website"))

secondary <- read.csv("../data/Council/Schools/Locations/secondaryschools.csv")
secondary %<>% separate(Location, c("lat", "lon"), sep = ",")
secondary %<>% mutate(properURL = cleanURL(More.information, "School website"))

special <- read.csv("../data/Council/Schools/Locations/specialschools.csv")
special %<>% separate(Location, c("lat", "lon"), sep = ",")
special %<>% mutate(properURL = cleanURL(More.information, "School website"))

# School catchment_areas (CEC)
cath_primary_areas <- readOGR(dsn = "../data/Council/Schools/Catchment areas", layer = "Primary_school_catchments_Roman_Catholic") %>% spTransform(CRSobj = CRS_ref)
cath_secondary_areas <- readOGR(dsn = "../data/Council/Schools/Catchment areas", layer = "Secondary_school_catchments_Roman_Catholic") %>% spTransform(CRSobj = CRS_ref)
nondem_primary_areas <- readOGR(dsn = "../data/Council/Schools/Catchment areas", layer = "Primary_school_catchments_non_denominational") %>% spTransform(CRSobj = CRS_ref)
nondem_secondary_areas <- readOGR(dsn = "../data/Council/Schools/Catchment areas", layer = "Secondary_school_catchments_non_denominational") %>% spTransform(CRSobj = CRS_ref)

# Boundary data

# From OS OpenData Boundary-Line dataset
boundaries_unitary <- readOGR("../data/OS/bdline_essh_gb/Data/GB", layer = "district_borough_unitary_region") %>% spTransform(CRSobj = CRS_ref)
boundaries_unitary <- boundaries_unitary[ldp, ]
boundaries_unitary$NAME <- factor(boundaries_unitary$NAME)
boundaries_west <- readOGR("../data/OS/bdline_essh_gb/Data/GB", layer = "westminster_const_region") %>% spTransform(CRSobj = CRS_ref)
boundaries_west <- boundaries_west[ldp, ]
boundaries_west$NAME <- factor(boundaries_west$NAME)
boundaries_const <- readOGR("../data/OS/bdline_essh_gb/Data/GB", layer = "scotland_and_wales_const_region") %>% spTransform(CRSobj = CRS_ref)
boundaries_const <- boundaries_const[ldp, ]
boundaries_const$NAME <- factor(boundaries_const$NAME)

boundaries_ward <- readOGR("../data/OS/bdline_essh_gb/Data/GB", layer = "district_borough_unitary_ward_region") %>% spTransform(CRSobj = CRS_ref)
boundaries_ward$CODE %<>% as.character()

# Get England and Wales 2011 population data from InFuse
# Note: this data looks a bit strange, with a few exceptionally high densities, and the numbers of GB wards (8,000+) do not seem to tally with the number of polygons in the shapefile (7,000+). This data is only used to calculate a rough approximation of the GB ward density ranks in 2011
boundaries_ward_ew <- read_csv("../data/UKDS Census/GB/Data_AGE_UNIT.csv")
boundaries_ward_ew %<>% rename(pop_2011 = F167, CODE = GEO_CODE)
boundaries_ward_ew %<>% filter(!is.na(CODE))
boundaries_ward_ew$CODE %<>% as.character()
boundaries_ward_ew %<>% filter(!is.na(pop_2011))
boundaries_ward_ew %<>% select(CODE, pop_2011)

# Scottish ward mid-year population estimates (from old SNS website)
boundaries_ward_pop <- read_csv("../data/SNS/ward/Batch1_MWC0R0_23_1_2016.csv")
names(boundaries_ward_pop) <- c("CODE","pop_2001","pop_2002","pop_2003","pop_2004","pop_2005","pop_2006","pop_2007","pop_2008","pop_2009","pop_2010","pop_2011","pop_2012","pop_2013")
boundaries_ward_pop %<>% filter(CODE != "GeographyCode")

# Merge Scotland with E&W
boundaries_ward_pop <- full_join(boundaries_ward_pop, boundaries_ward_ew, by = "CODE")
boundaries_ward_pop$pop_2011 = ifelse(!is.na(boundaries_ward_pop$pop_2011.x), boundaries_ward_pop$pop_2011.x, boundaries_ward_pop$pop_2011.y)
boundaries_ward_pop %<>% select(-pop_2011.x, -pop_2011.y)
boundaries_ward_pop$pop_2011 <- as.integer(boundaries_ward_pop$pop_2011)
rm(boundaries_ward_ew)

# Add to spatial data
boundaries_ward@data <- left_join(boundaries_ward@data, boundaries_ward_pop)
CRS(proj4string(boundaries_ward))
# rgeos::gArea does not work here as it expects a projection and here +proj=lonlat, use geosphere::areaPolygon instead
boundaries_ward@data$area <- geosphere::areaPolygon(boundaries_ward)
boundaries_ward@data %<>% mutate(pop_density = pop_2011 / (area/1000000), pop_density_rank_gb = min_rank(-pop_density))
View(boundaries_ward@data)

# Keep Scotland only
boundaries_ward <- boundaries_ward[str_detect(boundaries_ward$CODE, "S"),]
boundaries_ward@data %<>% mutate(pop_density = pop_2013 / (area/1000000), pop_density_rank = min_rank(-pop_density), pop_density_min = min(pop_density), pop_density_max = max(pop_density), pop_density_mean = sum(pop_2013) / sum(area/1000000), pop_density_median = median(pop_density))

# Keep LDP area only
boundaries_ward <- boundaries_ward[ldp, ]
boundaries_ward$NAME <- factor(boundaries_ward$NAME)
#boundaries_ward$council_number[boundaries_ward$FILE_NAME == "CITY_OF_EDINBURGH"] <- rank(boundaries_ward$NAME[boundaries_ward$FILE_NAME == "CITY_OF_EDINBURGH"])
# Need to hardcode here as annoyingly the ward numbers used on the Council website do not correspond to the alphabetical order of the ward names
boundaries_ward$council_number <- c(6, 15, 5, 2, 7, 13, 9, 8, 12, 3, 14, 16, 18, 4, 10, 11, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1)
boundaries_ward$URL <- cleanURL(ifelse(is.na(boundaries_ward$council_number), NA, paste(sep= "", "http://www.edinburgh.gov.uk/councillors/specificWard/", boundaries_ward$council_number, "/")), "Councillors contact details")
View(boundaries_ward@data)
rm(boundaries_ward_pop)

# Other admin boundaries (CEC)
boundaries_cc <- readOGR(dsn = "../data/Council/boundaries_cc", layer = "Community_councils") %>% spTransform(CRSobj = CRS_ref)
boundaries_nn <- readOGR(dsn = "../data/Council/boundaries_nn", layer = "Natural_neighbourhoods") %>% spTransform(CRSobj = CRS_ref)
boundaries_np <- readOGR(dsn = "../data/Council/boundaries_np", layer = "Neighbourhood_partnerships") %>% spTransform(CRSobj = CRS_ref)

# Community Council online presence data from Bruce Ryan (originally Improvement Service)
links_cc <- read_excel("../data/Bruce/2015 analysis (IS data) Edinburgh only.xlsx") %>% 
  select(CC, Website, Facebook, Twitter) %>% 
  filter(CC != "Number of CCs: 46") %>% 
  mutate_each(funs(ifelse(. == "N/A", NA, .))) %>% 
  mutate(LABEL = str_replace(CC, "&", "and")) %>% 
  mutate(LABEL = str_replace(LABEL, "Ratho and district", "Ratho and District")) %>% 
  mutate(LABEL = str_replace(LABEL, "Sighthill/Broomhouse/Parkhead", "Sighthill, Broomhouse and Parkhead")) %>%
  mutate(LABEL = str_replace(LABEL, "Grange Prestonfield", "Grange/Prestonfield"))
# For some reason, need to hardcode the following
links_cc$LABEL[16] <- "Grange/Prestonfield"
boundaries_cc@data <- left_join(boundaries_cc@data, links_cc)
boundaries_cc@data %<>% mutate(properURL = cleanURL(Link, "Link to contact details on CEC website"), properWebsite = cleanURL(Website, "Link to Community Council website"), properFacebook = cleanURL(Facebook, "Link to Community Council Facebook page"), properTwitter = cleanURL(Twitter, "Link to Community Council Twitter feed"))
boundaries_cc@data %<>% select(OBJECTID, LABEL, properURL, properWebsite, properFacebook, properTwitter)
# boundaries_cc@data %>% View()
rm(links_cc)

# Postcode sectors from UK Data service - Census Support (2011: OGL, 2001: Derived)
boundaries_pcs <- readOGR("../data/UKDS Census/Boundary/Scotland_pcs_2011", layer = "scotland_pcs_2011") %>% spTransform(CRSobj = CRS_ref)
boundaries_pcs <- boundaries_pcs[ldp, ]
boundaries_pcs$NAME <- factor(boundaries_pcs$NAME)

# SNS data

# SNS 2011 data zone boundaries
boundaries_dz_2011 <- readOGR("../data/SNS/SG_DataZoneBdry_2011", layer = "SG_DataZone_Bdry_2011") %>% spTransform(CRSobj = CRS_ref)
CRS(proj4string(boundaries_dz_2011))
# rgeos::gArea does not work here as it expects a projection and here +proj=lonlat, use geosphere::areaPolygon instead
boundaries_dz_2011@data$area <- geosphere::areaPolygon(boundaries_dz_2011)
boundaries_dz_2011@data %<>% mutate(pop_density = TotPop2011 / (area/1000000), pop_density_rank = min_rank(-pop_density), pop_density_min = min(pop_density), pop_density_max = max(pop_density), pop_density_mean = sum(TotPop2011) / sum(area/1000000), pop_density_median = median(pop_density))
boundaries_dz_2011 <- boundaries_dz_2011[ldp,]

# SNS 2001 data zone boundaries
boundaries_dz_2001 <- readOGR("../data/UKDS Census/Boundary/Scotland_dz_2001", layer = "scotland_dz_2001") %>% spTransform(CRSobj = CRS_ref)
# for some unfathomable reason, there are some duplicates
boundaries_dz_2001 <- boundaries_dz_2001[which(!duplicated(boundaries_dz_2001$ZONECODE)),] 
boundaries_dz_2001@data %<>% mutate(ZONECODE = as.character(ZONECODE))

# 2013 housing data for 2001 data zones
housing_dz_2001 <- read_csv("../data/SNS/dz/house-sales-prices.csv", skip = 7)
names(housing_dz_2001) <- c("Purl", "ZONECODE", "housing_prices_2013_lower_quartile", "housing_prices_2013_mean", "housing_prices_2013_median", "housing_prices_2013_upper_quartile")
housing_dz_2001 %<>% select(ZONECODE, housing_prices_2013_mean, housing_prices_2013_median)
boundaries_dz_2001@data <- left_join(boundaries_dz_2001@data, housing_dz_2001)
boundaries_dz_2001@data %<>% mutate(housing_prices_2013_rank = min_rank(-housing_prices_2013_median))
rm(housing_dz_2001)

# 2012 SIMD data
SIMD_dz_2001 <- read_csv("../data/SNS/dz/Batch2_2012_ZNC0R0_23_1_2016.csv")
colnames(SIMD_dz_2001) <- c("ZONECODE", "SIMD_2012_employment", "SIMD_2012_health", "SIMD_2012_housing", "SIMD_2012_income", "SIMD_2012")
SIMD_dz_2001 %<>% filter(ZONECODE != "GeographyCode")
boundaries_dz_2001@data <- left_join(boundaries_dz_2001@data, SIMD_dz_2001)
SIMD_dz_2001 <- read_csv("../data/SNS/dz/Batch1_2012_ZNC0R0_23_1_2016.csv")
colnames(SIMD_dz_2001) <- c("ZONECODE", "SIMD_2012_drive_times", "SIMD_2012_public_transport", "SIMD_2012_access_to_services", "SIMD_2012_crime", "SIMD_2012_education", "SIMD_2012_employment")
SIMD_dz_2001 %<>% filter(ZONECODE != "GeographyCode")
SIMD_dz_2001 %<>% select(-SIMD_2012_employment)

boundaries_dz_2001@data <- left_join(boundaries_dz_2001@data, SIMD_dz_2001)
boundaries_dz_2001 <- boundaries_dz_2001[ldp,]
rm(SIMD_dz_2001)


# Save the resulting datasets in one file for loading into the app
#save.image(file = "./app/data/payload.RData")

rm(list = ls())

load(file = "./app/data/payload.RData")
