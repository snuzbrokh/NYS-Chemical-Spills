library(rmapshaper)
library(rgdal)
library(tidyverse)
require(sf)

# Read in NYS shape files
#nys <- read_sf('./geodata',"nys_simplified")

# Read in bulk facility data

storage <- read_csv('./data/storage_clean.csv')

# Read in spill data

spills <- read_csv('./data/spills_clean.csv')

# Combine bulk facility chemical list with spill data
# to look only at chemicals that are in the storage dataset

storage_and_spills = storage %>% 
  inner_join(spills, by=c('Facility.Name','County','DEC.Region'), suffix=c(".facility",".spill"))

storage = storage %>% 
  semi_join(spills, by=c('Facility.Name','County','DEC.Region'))


# Read in Latitude and Longitude (jittered)
storage$Lat = jitter(storage$Lat)
storage$Lon = jitter(storage$Lon)

spills$Lat = jitter(spills$Lat, amount=0.05)
spills$Lon = jitter(spills$Lon, amount =0.05)

# Reclassify DEC Regions as Factors
# storage$DEC.Region = as.factor(storage_clean$DEC.Region)
# spills$DEC.Region = as.factor(spills_clean$DEC.Region)

# Format Choices
materials = spills$Material %>% unique() %>% sort()
material_family = spills$`Material.Family` %>% unique() %>% sort()
sources = spills$Source_ %>% unique() %>% sort()
spill_counties = spills$County %>% unique() %>% sort()

sites = storage$Site.Type %>% unique() %>% sort()
localities = storage$Locality %>% unique() %>% sort()
tank_locations = storage$Tank.Location %>% unique()
counties = storage$County %>% unique() %>% sort()
decs = storage$`DEC.Region` %>% unique() %>% sort()

################ Interactive Map Plots ################ ################ ################ ##########
colors = c('Greenhouse Gas'="#005300",
           'Petroleum'="black",
           'Hazardous Material'="#c4200e",
           'Petrochemical'="#3D1E00",
           'Commodity Chemical'="#BE002E")

pal = leaflet::colorFactor(palette = colors, domain = as.factor(storage$Material.Family))

font <- list(
  size = 40,
  color = "white"
)

label <- list(
  bgcolor = "#232F34",
  bordercolor = "transparent",
  font = font
)

################ County Spills by Material ################ ################ #
county_spills = function(spills) {
  
  font <- list(
    size = 20,
    color = "white"
  )
  
  label <- list(
    bordercolor = "transparent",
    font = font
  )
  
  g = ggplot(data = spills, 
            aes(x = reorder(`Material`,total_spilled), y = `total_spilled`,
               text = paste0(toupper(`Material`), ": ",round(total_spilled,-2)," Gallons"))) +
    geom_point() +
    scale_y_continuous(trans='log10') +
    coord_flip() +
    labs(title='Total Material Spills by Chemical (Gal)',
         x= NULL,
         y = NULL) +
    scale_fill_brewer(palette='Set1') +
    theme_bw() +
    theme(legend.key=element_blank())
  ggplotly(g, tooltip = c("text")) %>% style(hoverlabel = label) %>%
    layout(font = font, legend = list(size=12))
}


################ County Spills by Source ################ ################ #
spill_sources = function(spills) {
  
  font <- list(
    size = 14,
    color = "white"
  )
  
  label <- list(
    bordercolor = "transparent",
    font = font
  )
  
  
  g = ggplot(data = spills, 
             aes(x = reorder(`Source_`,total_spilled), y = `total_spilled`,
                 text = paste0(round(total_spilled,0)," Gallons of ",Material.Family))) +
    geom_col(aes(fill = Material.Family),alpha=0.7) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans='log10') +
    coord_flip() +
    labs(title='Total Spills by Source and Chemical Family',
         x= NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.key=element_blank())
  ggplotly(g, tooltip = c("text")) %>% style(hoverlabel = label) %>%
    layout(font = font, legend = list(size=12))
}
