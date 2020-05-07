library(rmapshaper)
library(rgdal)
library(tidyverse)


# Read in NYS shape files
nys <- readOGR('./geodata',"nys_simplified")

# Read in data
storage <- read_csv('./data/storage_clean.csv')

# Read in Latitude and Longitude (jittered)
storage$Lat = jitter(storage$Lat)
storage$Lon = jitter(storage$Lon)

# Format Material Choices
materials = storage$Material.Name %>% unique() %>% sort()
sites = storage$Site.Type.Name %>% unique() %>% sort()
localities = storage$Locality %>% unique() %>% sort()
tank_locations = storage$Tank.Location %>% unique()
counties = storage$County.Name %>% unique() %>% sort()

# Dates of Facility Installation
min_date = as.Date(min(storage$Install.Date),"%Y-%m-%d")
max_date = as.Date(max(storage$Install.Date),"%Y-%m-%d")
