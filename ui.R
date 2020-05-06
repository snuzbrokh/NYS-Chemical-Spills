library(DT)

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
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


bootstrapPage(
  navbarPage(theme = shinytheme("flatly"),collapsible=TRUE, "Storage", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        #tags$head(includeCSS("styles.css")),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("map",width="100%"),
                        
                        absolutePanel(id="controls", class="panel panel-default",
                                      fixed = TRUE, draggable = FALSE, top = "auto",
                                      left = 'auto', right = "auto", bottom = "auto",
                                      width = "auto", height = "auto",
                                      
                                      h3("Storage Explorer"),
                                      checkboxGroupInput("site-status", 
                                                         h4("Site Status"), 
                                                         choices = list("Closed" = "Unregulated/Closed", 
                                                                        "Inactive" = "Inactive" , 
                                                                        "Active" = "Active"),
                                                         selected = c("Active","Inactive","Unregulated/Closed")
                                                         ),
                                      pickerInput(
                                        'materials', 'Materials', 
                                        choices = materials, 
                                        options = list(`actions-box` = TRUE, 
                                                       `none-selected-text` = "Please make a selection!",
                                                       dropupAuto = FALSE,
                                                       `data-live-search` = TRUE),
                                        selected = storage$County.Name,
                                        multiple = FALSE),
                                      plotOutput("cumgrowth", height="200px", width="100%"),
                                      plotOutput("histCentile", height = 200,width="100%"),
                                      plotOutput("topLoc", height = 300,width="100%")
                                     
                                      
                    )
           ),
           tabPanel("Plots",
                    sidebarLayout(
                      sidebarPanel(

                        pickerInput("county_select", "County:",
                                    choices = counties, 
                                    options = list(`actions-box` = TRUE, 
                                                   `none-selected-text` = "Please make a selection!"),
                                    selected = storage$County.Name,
                                    multiple = TRUE)
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel(plotOutput("histogram", height = 300))
                        )
                      )
                      )
                    )
)
))
           # tabPanel("Data Explorer",
           #          fluidRow(
           #              column(3,
           #                     selectInput("localities","Localities",c("All localities"="",storage$Locality), multiple=TRUE)
           #                     ),
           #              column(3,
           #                     selectInput("sites","Sites",c("All sites"=""), multiple=TRUE)
           #              ),
           #              column(3,
           #                     selectInput("materials","Materials",c("All materials"=""), multiple=TRUE)
           #              )
           #          ),
           #          hr(),
           #          DT::dataTableOutput("table")
           #          ),
           # conditionalPanel("false", icon("crosshair"))
           # )