path1<-"X/maps and shapefiles"
path2<-"X/Other Survey reports/NFHS all factsheets"

#Path1 contains the shape file (India States and Districts )
#Path2 contains the CSV that is linked to the NFHS dashboard
#Geojson files are needed for HTML and CSS to generate choropleths using the NFHS data)

#Install the appropriate package (to read geojson file)
#install.packages("sf")
#install.packages("dplyr")
library(sf)

# First open the shape file containing Indian States
shapefile_state <- st_read(paste0(path1, "/", "India_States.geojson"))
plot1<-plot(st_geometry(shapefile_state))

#What column names are available in this file? 
names(shapefile_state)

#There appears to be both state name (ST_NM) and state id (ID)
sort(unique(shapefile_state$ST_NM))

#################################################################
#Now let us compare State Names in both the shapefile and the csv 
#################################################################

#First, Import the CSV which is now the front_end for the NFHS dashboard 
nfhs_backend<-read.csv((paste0(path2, "/", "nfhs_all_data.csv")))


# Extract state names from each source
sheet_states <- sort(unique(nfhs_backend$Geography[nfhs_backend$Geo.Level == "State"])) 
##^The geography column contains district names also hence 
shp_states   <- sort(unique(shapefile_state$ST_NM))

# How many in each?
length(sheet_states)  # should be ~36
length(shp_states)

# In sheet but NOT in shapefile → these won't get coloured on the map
setdiff(sheet_states, shp_states)

# In shapefile but NOT in sheet → these will be grey/empty on the map  
setdiff(shp_states, sheet_states)

#How many unique states are there?
unique_states<-length(unique(shapefile_state$ST_NM))
print(unique_states)

#Corrections are needed in spellings: Andaman and Nicobar, NCT of Delhi 

library(dplyr)

# Step 1: fix the typo and merge the two UTs into one geometry (in 2020, Dadra, Nagar Haveli, Daman and Diu Merged into one union territory)
shapefile_state <- shapefile_state %>%
  mutate(ST_NM = recode(ST_NM,
                        "Andaman & Nicobar Island" = "Andaman & Nicobar Islands",
                        "Dadara & Nagar Havelli"   = "Dadra & Nagar Haveli and Daman & Diu",
                        "Daman & Diu"              = "Dadra & Nagar Haveli and Daman & Diu",
                        "NCT of Delhi"             = "NCT Delhi"
  )) %>%
  st_make_valid() %>%  # fix invalid geometries before union
  group_by(ST_NM) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# Verify — both should now return character(0)
setdiff(sheet_states, sort(unique(shapefile_state$ST_NM)))
setdiff(sort(unique(shapefile_state$ST_NM)), sheet_states)

# And count should now be 36
nrow(shapefile_state)

#We now have the final shapefile to use for the nfhsdashboard 
st_write(shapefile_state, (paste0(path2, "/", "choropleths", "/", "India_States_fornfhsdashboard.geojson")), append= FALSE)

#############################################################
#Now let us repeat the process for Indian Districts 
############################################################

# Open json file containing district Name

shapefile_district <- st_read(paste0(path1, "/", "India_districts.json"))
plot1<-plot(st_geometry(shapefile_district))

#somehow this is a json. Lets convert it into a geojson later on

#We need the column name that has district names 
names(shapefile_district)

#rename for consistency 
shapefile_district <- shapefile_district %>%
  rename(ST_NM = stname)

#We have to make corrections in state and district name. We already have the code 
#for making state level corrections above. Now lets find out district level corrections

sheet_districts <- sort(unique(nfhs_backend$Geography[nfhs_backend$Geo.Level == "District"])) 
##^The geography column contains district names also hence 
shp_districts <- sort(unique(shapefile_district$DISTRICT))

# In sheet but NOT in shapefile → these won't get coloured on the map
setdiff(sheet_districts, shp_districts)

# In shapefile but NOT in sheet → these will be grey/empty on the map  
setdiff(shp_districts, sheet_districts)


# ============================================================
# DISTRICT SHAPEFILE CLEANING: STANDARDIZE STATE NAMES
# ============================================================

crosswalk_states <- c(
  "Andaman & Nicobar Island" = "Andaman & Nicobar Islands",
  "Dadara & Nagar Havelli"   = "Dadra & Nagar Haveli and Daman & Diu",
  "Daman & Diu"              = "Dadra & Nagar Haveli and Daman & Diu",
  "NCT of Delhi"             = "NCT Delhi"
)

shapefile_district <- shapefile_district %>%
  mutate(ST_NM = recode(ST_NM, !!!crosswalk_states)) %>%
  st_make_valid()

# Verify
setdiff(sheet_states, sort(unique(shapefile_state$ST_NM)))
setdiff(sort(unique(shapefile_state$ST_NM)), sheet_states)


# ============================================================
# DISTRICT SHAPEFILE CLEANING: STANDARDIZE DISTRICT NAMES
# ============================================================

crosswalk_districts <- c(
  "Ahmadabad"                  = "Ahmedabad",
  "Ahmadnagar"                 = "Ahmednagar",
  "Almora\n"                   = "Almora",
  "Amroha"                     = "Jyotiba Phule Nagar",
  "Aravalli"                   = "Aravali",
  "Bametara"                   = "Bemetara",
  "Banas Kantha"               = "Banaskantha",
  "Bara Banki"                 = "Barabanki",
  "Bhadohi"                    = "Sant Ravidas Nagar (Bhadohi)",
  "Bhadradri"                  = "Bhadradri Kothagudem",
  "Chota Udaipur"              = "Chhota Udaipur",
  "Cooch Behar"                = "Koch Bihar",
  "Dadra & Nagar Haveli"       = "Dadra And Nagar Haveli",
  "Dakshin Bastar Dantewada"   = "Dantewada",
  "Darjiling"                  = "Darjeeling",
  "Devbhoomi Dwarka"           = "Devbhumi Dwarka",
  "Dohad"                      = "Dahod",
  "East Nimar"                 = "Khandwa (East Nimar)",
  "Garhwal"                    = "Pauri Garhwal",
  "Gariaband"                  = "Gariyaband",
  "Gurdaspur"                  = "Gurudaspur",
  "Gurugram"                   = "Gurgaon",
  "Hardwar"                    = "Haridwar",
  "Hooghly"                    = "Hugli",
  "Howrah"                     = "Haora",
  "Jagtial"                    = "Jagitial",
  "Jangaon"                    = "Jangoan",
  "Janjgir - Champa"           = "Janjgir-Champa",
  "Jayashankar"                = "Jayashankar Bhupalapally",
  "Jogulamba"                  = "Jogulamba Gadwal",
  "Kaimur (bhabua)"            = "Kaimur (Bhabua)",
  "Karbi Anglong East"         = "Karbi Anglong",
  "Karbi Anglong West"         = "West Karbi Anglong",
  "Kasganj"                    = "Kanshiram Nagar",
  "Komaram Bheem"              = "Komaram Bheem Asifabad",
  "Kondagaon"                  = "Kodagaon",
  "Leh(ladakh)"                = "Leh (Ladakh)",
  "Mahrajganj"                 = "Maharajganj",
  "Medchal Malkajgiri"         = "Medchal-Malkajgiri",
  "Narsimhapur"                = "Narsinghpur",
  "Nicobars"                   = "Nicobar",
  "North  & Middle Andaman"    = "North And Middle Andaman",
  "North  District"            = "North District",
  "North Twenty Four Pargan*"  = "North Twenty Four Parganas",
  "Panch Mahals"               = "Panchmahal",
  "Peddapalle"                 = "Peddapalli",
  "Rajanna"                    = "Rajanna Sircilla",
  "Rangareddy"                 = "Ranga Reddy",
  "Sabar Kantha"               = "Sabarkantha",
  "Sahibzada Ajit Singh Nag*"  = "Sahibzada Ajit Singh Nagar",
  "Sant Kabir Nagar"           = "Sant Kabeer Nagar",
  "Saraikela-kharsawan"        = "Saraikela-Kharsawan",
  "Shrawasti"                  = "Shravasti",
  "Sipahijala"                 = "Sepahijala",
  "South Salmara-mankachar"    = "South Salmara Mancachar",
  "South Twenty Four Pargan*"  = "South Twenty Four Parganas",
  "Sri Potti Sriramulu Nell*"  = "Sri Potti Sriramulu Nellore",
  "Udham Singh Nagar"          = "Udam Singh Nagar",
  "Unokoti"                    = "Unakoti",
  "West Nimar"                 = "Khargone (West Nimar)",
  "Yadadri"                    = "Yadadri Bhuvanagiri"
)

shapefile_district <- shapefile_district %>%
  mutate(DISTRICT = recode(DISTRICT, !!!crosswalk_districts)) %>%
  st_make_valid() %>%
  group_by(ST_NM, DISTRICT) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Verify
setdiff(sheet_districts, sort(unique(shapefile_district$DISTRICT)))
setdiff(sort(unique(shapefile_district$DISTRICT)), sheet_districts)


# ============================================================
# EXPORT CLEANED SHAPEFILES TO GEOJSON
# ============================================================

#st_write(shapefile_state,    paste0(path1, "/", "India_States_clean.geojson"),    delete_dsn = TRUE)
st_write(shapefile_district, (paste0(path2, "/", "choropleths", "/", "India_Districts_fornfhsdashboard.geojson")), delete_dsn = TRUE )


unique_states <- unique(shapefile_district$ST_NM)

for (state in unique_states) {
  
  #Export each state
  state_districts <- shapefile_district %>%
    filter(ST_NM == state)
  
  # Clean state name for filename (remove special characters)
  state_filename <- gsub("[^A-Za-z0-9]", "_", state)

st_write(
  state_districts,
  paste0(path2, "/","choropleths", "/", state, "_fornfhsdashboard", ".geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Exported: ", state)
}



  
