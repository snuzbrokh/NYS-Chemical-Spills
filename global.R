library(rmapshaper)
library(rgdal)
library(tidyverse)


# Read in NYS shape files
nys <- readOGR('./geodata',"nys_simplified")

# Read in bulk facility data

storage <- read_csv('./data/storage_clean.csv')


# Read in spill data

spills <- read_csv('./data/spills_clean.csv')

# Combine bulk facility chemical list with spill data
# to look only at chemicals that are in the storage dataset

storage = storage %>% 
  semi_join(spills, by = c('Material.Name' = 'Material Name'))
spills = spills %>% 
  semi_join(storage, by = c('Material Name' = 'Material.Name'))





# Read in Latitude and Longitude (jittered)
storage$Lat = jitter(storage$Lat)
storage$Lon = jitter(storage$Lon)


# Format Material Choices
materials = storage$Material.Name %>% unique() %>% sort()
material_family = storage$`Material Family` %>% unique() %>% sort()
sites = storage$Site.Type.Name %>% unique() %>% sort()
localities = storage$Locality %>% unique() %>% sort()
tank_locations = storage$Tank.Location %>% unique()
counties = storage$County.Name %>% unique() %>% sort()

spill_counties = spills$County %>% unique() %>% sort()

# Dates of Facility Installation
min_date = as.Date(min(storage$Install.Date),"%Y-%m-%d")
max_date = as.Date(max(storage$Install.Date),"%Y-%m-%d")


################ Interactive Map Plots ################ ################ ################ ##########
colors = c("#BE002E","#FFFF00","#005300","#3D1E00","#151515")

font <- list(
  family = "Roboto Condensed",
  size = 40,
  color = "white"
)

label <- list(
  bgcolor = "#232F34",
  bordercolor = "transparent",
  font = font
)


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
            aes(x = reorder(`Material Name`,total_spilled), y = `total_spilled`,
               text = paste0(toupper(`Material Name`), ": ",round(total_spilled,-2)," Gallons"))) +
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



spill_sources = function(spills) {
  colors = c("Petroleum" = colors[5], 
              "Greenhouse Gas" = colors[3], 
              "Hazardous Material" = colors[1], 
              "Petrochemical" = colors[4], 
              "Commodity Chemical" = colors[2])
  
  font <- list(
    size = 20,
    color = "white"
  )
  
  label <- list(
    bordercolor = "transparent",
    font = font
  )
  
  
  g = ggplot(data = spills, 
             aes(x = reorder(`Source`,total_spilled), y = `total_spilled`,
                 text = paste0(round(total_spilled,0)," Gallons of ",`Material Family`))) +
    geom_col(aes(fill=`Material Family`)) +
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
