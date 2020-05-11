library(DT)

if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")



# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)); }
    })
  }
}"
# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('spill_range', sci = false)
  }, 5)})
"

bootstrapPage(
  navbarPage(theme = shinytheme("spacelab"),collapsible=TRUE, "Chemical Spills and Facilities in NYS", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        tags$head(tags$script(HTML(JS.logify))),
                        tags$head(tags$script(HTML(JS.onload))),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("map",width="100%",height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      top = 120, left = 40, width = "auto", fixed=TRUE,
                                      draggable = TRUE, height = "auto",
                                      
                                      style = "font-size: 16px !important;",
                                      prettyCheckboxGroup("site-status", 
                                                         h4("Facility Status"), 
                                                         choices = list("Closed" = "Unregulated/Closed", 
                                                                        "Inactive" = "Inactive" , 
                                                                        "Active" = "Active"),
                                                         selected = c("Active","Inactive","Unregulated/Closed"),
                                                         inline = TRUE
                                                         ),
                                      
                                      pickerInput("material_family_select", h4("Material Family:"),   
                                                  choices = material_family, 
                                                  selected = c("Petroleum"),
                                                  options = list(`actions-box` = TRUE,
                                                                 inline = TRUE),
                                                  multiple = TRUE),
                                      pickerInput(
                                        'material_name_select', h4('Material'), 
                                        choices = NULL, 
                                        options = list(`actions-box` = TRUE, 
                                                       `live-search` = TRUE,
                                                       inline = TRUE),
                                        selected = NULL,
                                        multiple = TRUE),
                                      
                                      sliderInput("spill_range", h4("Spill Size Range (Gal):"), 
                                                  min = 0, max = 8, step = 1, value = c(1,4)),
                                      
                                      actionButton("show-facilities", "Show Facilities"),
                                      actionButton("show-spills", "Show Spills"),
                                      actionButton("clear_map", "Clear Map")
                    )
                    )
           ),
           
           tabPanel
           (
             "County Plots",
              sidebarLayout(
                sidebarPanel(
                  span(tags$i(h6("Investigation of chemical spills by County")), style="color:#045a8d"),
                  
                  sliderInput("result_range", "Number of Results:", 
                              min = 0, max = 50, value = c(0,10)),
                  
                  pickerInput("dec_select", "DEC Region:",   
                              choices = decs, 
                              selected = c("1"),
                              options = list(`actions-box` = TRUE, 
                                             `data-live-search` = TRUE,
                                             inline = TRUE),
                              multiple = TRUE),
                  pickerInput("county_select", "County:",   
                              choices = NULL, 
                              selected = NULL,
                              options = list(`actions-box` = TRUE, 
                                             `data-live-search` = TRUE,
                                             inline = TRUE),
                              multiple = TRUE),
                  
                  
                  pickerInput("county_material_family_select", "Material Family:",   
                              choices = material_family, 
                              selected = material_family,
                              options = list(`actions-box` = TRUE,
                                             inline = TRUE),
                              multiple = TRUE),
                  plotlyOutput("violin")
                  
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("County Spills", plotlyOutput("plot_county_spills")),
                    tabPanel("Spill Sources", plotlyOutput("plot_spill_sources"))
                  ),
                  DT::dataTableOutput("table")
                )
              )
           ),
           
           tabPanel
           (
             "Analysis",
             sidebarLayout(
               sidebarPanel(
                 span(tags$i(h3("Investigation of Responsiveness of DEC Regional Offices to Chemical Spills")), style="color:#045a8d"),
                 
                 pickerInput("time_unit_select", "Time Unit:",   
                             choices = c("Month","Quarter","HalfYear","Year"), 
                             selected = "Year",
                             options = list(`actions-box` = TRUE, 
                                            `data-live-search` = TRUE,
                                            inline = TRUE,
                                            heder = "TIME UNIT"),
                             multiple = FALSE),
                 pickerInput("dec_analysis_select", "DEC Region:",   
                             choices = decs, 
                             selected = c("1"),
                             options = list(`actions-box` = TRUE, 
                                            `data-live-search` = TRUE,
                                            inline = TRUE),
                             multiple = FALSE),
                 
                 pickerInput("county_analysis_select", "County:",   
                             choices = NULL, 
                             selected = NULL,
                             options = list(`actions-box` = TRUE, 
                                            `data-live-search` = TRUE,
                                            inline = TRUE),
                             multiple = TRUE),
                 
                 pickerInput("material_family_analysis_select", "Material Family:",   
                             choices = material_family, 
                             selected = material_family,
                             options = list(`actions-box` = TRUE,
                                            inline = TRUE),
                             multiple = TRUE),
                 
                 pickerInput("source_select", "Sources of Spills:",   
                             choices = sources, 
                             selected = sources,
                             options = list(`actions-box` = TRUE, 
                                            `data-live-search` = TRUE,
                                            inline = TRUE),
                             multiple = TRUE)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Regional Report Lag", plotlyOutput("analysis_plot"))
                 )
             )
           )
          ),
          
           tabPanel
           (
             "About this site",
              tags$div(
                
                tags$h4("Contact"),"snuzbrokh@gmail.com",
                
                tags$br(),tags$br(),tags$h4("Background"), 
                
                "This visualization shows information about bulk storage facilities and 
                tanks that are currently being operated or were operated in the State of New York.
                There are four bulk storage facilities: Petroleum Bulk Storage (PBS), Chemical Bulk Storage (CBS), Major Oil Storage Facilities (MOSF), 
                and Liquefied Natural Gas (LNG).",
                
                tags$br(),tags$br(),tags$h4("Environmental Standards"),
                
                "Tanks storing petroleum and hazardous chemicals must meet minimum standards established by the United States Environmental Protection Agency (USEPA) 
                and the New York State Department of Environmental Conservation (NYSDEC). New York's Hazardous Substances Bulk Storage Program provides guidelines 
                and controls for the storage of many different hazardous chemicals. Improper handling and storage of petroleum and hazardous chemicals can result 
                in leaks and spills and pose a serious threat to the quality of the environment in New York State. Petroleum, additives, and a variety of 
                industrial chemicals have been discovered in many of the State's groundwater supplies. In some wells, only trace quantities have been 
                discovered; in others, levels have exceeded federal and State drinking water standards. Many drinking water supplies have been closed because 
                of excessive chemical contamination.",
                
                tags$br(),tags$br(),
                
                "Water contamination is only one consequence of poor handling practices. Mismanagement of some substances may pose occupational hazards, present a 
                fire or explosion risk, or result in a release of odors or fumes with public health and environmental consequences to the neighboring community.",
                
                tags$br(),tags$br(),
                
                "Gasoline, which fuels the millions of automobiles we all drive each day, is highly flammable and can flash violently when ignited. Gasoline and 
                many other hazardous chemicals when inhaled can cause drowsiness, nausea, and other adverse health effects. Once petroleum or a chemical soaks into 
                the ground, it disperses and may dissolve and contaminate a water supply for many years. Cleanup is often difficult and it is usually very expensive.",
                
                tags$br(),tags$br(),
                
                "New York State has approximately 46,000 storage facilities which involve an estimated 108,000 bulk storage tanks. Leaks and spills occur as a result 
                of poor housekeeping, overfilling of tanks, loading and unloading mistakes, and poor maintenance and inspection.",
                
                tags$br(),tags$br(),tags$h4("Prevention"),
                
                "New York State prevents leaks and spills at petroleum and chemical storage facilities through the Bulk Storage Program operated by NYSDEC. 
                The Bulk Storage Program is based on four laws enacted over the past 20 years. Three are State laws requiring NYSDEC to develop and enforce standards 
                for storage and handling of petroleum and chemical products and to regulate aboveground and underground tanks storing these products. The fourth law 
                is the federal amendment to RCRA (Subtitle I) requiring USEPA to regulate underground storage tanks (USTs). Under a memorandum of understanding with 
                USEPA, NYSDEC assists in the regulation and inspection of 23,000 underground tanks covered by Subtitle I, provides education to tank owners, and 
                encourages compliance with federal regulations.
                ",
                
                tags$br(),tags$br(),tags$h4("Limitations of Data & Visualization"),
                
                "Installation, closing, and expiration dates are based on DEC’s best available information from historical and current sources/records.",
                
                tags$br(),tags$br(),
                
                "Facilities which have not yet been mapped have no coordinates.",
                
                tags$br(),tags$br(),
                
                "For bulk storage facilities including PBS, CBS, and MOSF, some information is not released because NYSDEC has determined that releasing the information 
                could endanger the life or safety of persons or the security of critical infrastructure. This information is withheld in accordance with the Public Officers 
                Law (POL §§86.5, 87.2(f), 89.5(a)(1)(1-a)). This includes tank information such as tank capacities, products stored, tank type, piping type, etc.). This 
                applies to all CBS and MOSF facilities and to PBS facilities at certain facility types (e.g., utilities, airports, storage terminals, etc.) and those that 
                store more than 10,000 gallons of flammable petroleum products in aboveground tanks. For these facilities, the information released is consistent with what 
                is provided on the NYDEC’s website including facility name, address, facility status (i.e., active, closed), and the date that the facility registration/license 
                expires.",
                
                tags$br(),tags$br(),
                
                "New LNG facility permits are now required since the laws and regulations (Environmental Conservation Law Article 23 Title 17 and 6 NYCRR Part 570) became 
                effective in February 2015. LNG facilities may be added as permits are issued.",
                
                tags$br(),tags$br(),
                
                "NYSDEC does not maintain the PBS registration records for the following 
                five counties which are delegated to manage their own records: Cortland, Nassau, Rockland, Suffolk, and Westchester.",
                
                
                tags$br()
              )
           )
))
