library(DT)
library(shiny)
library(leaflet)
library(ggplot2)
library(scales)
library(RColorBrewer)
require(Hmisc)
require(psych)

mapbox_url = 'https://api.mapbox.com/styles/v1/snuzbrokh/ck9txosrr0ij41iqmmcgby9q7/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic251emJyb2toIiwiYSI6ImNrOXR4Y3V5ejBkYjYzZG96aWlidW11NmsifQ.ZNl03PeSqd0DQwur0AuM3A'
set.seed(100)

function(input, output, session) {
    
    ####Interactive Map & Functions ################ ################ ################ ######
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(urlTemplate = mapbox_url) %>%
            #addPolygons(weight=0.5,smoothFactor=0.5,fillOpacity=0.05, highlightOptions=highlightOptions(color = "white",weight = 3,bringToFront = TRUE)) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Zoom to Level 1",
                onClick=JS("function(btn, map){ map.setZoom(6);}"))) %>% 
            setView(lng = -74.2179, lat = 43.2994, zoom = 7)	
        })
    
    ################ Facility Mapper ################ ################ #############
    observe({
        if (input$`show-facilities` == 0)
            return()
        
        
        isolate({
            if (nrow(facilityInBounds()) == 0)
                return(NULL)
            data = facilityInBounds()
            # domain = filteredData() %>% unique() %>% n_distinct()
            # 
            # pal_ = brewer.pal(domain,name='Dark2')
            
            leafletProxy("map", data = data) %>%
                clearMarkers() %>%
                addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = ~log(Quantity), 
                                 stroke=TRUE, color = "green",
                                 label = ~label,
                                 labelOptions = labelOptions(direction = "bottom",
                                                             style = list(
                                                                 "color" = "green",
                                                                 "font-family" = "Helvetica Neue",
                                                                 "font-style" = "bold",
                                                                 "font-size" = "18px"
                                                             )),
                                 fillColor = ~pal(Material.Family)) 
           
        })
        

    })
    
    ################ Spills Mapper ################ ################ #############
    observe({
        if(input$`show-spills` == 0)
            return()
        isolate({
            if (nrow(spillInBounds()) == 0)
                return(NULL)
            data = spillInBounds()
            leafletProxy("map", data = data) %>%
                addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = ~log(Quantity), 
                                 stroke=TRUE, color = 'red',
                                 label = ~label,
                                 labelOptions = labelOptions(direction = "bottom",
                                                             style = list(
                                                                 "color" = "red",
                                                                 "font-family" = "Helvetica Neue",
                                                                 "font-style" = "bold",
                                                                 "font-size" = "18px"
                                                             )),
                                 fillColor = ~pal(Material.Family))
        })
    })
    
    
    ################ Clear Map ################ ################ #############
    observe({
        if (input$clear_map == 0)
            return()
        isolate({
            leafletProxy("map") %>%
                clearMarkers()
        })
    })
    
    # A reactive expression that returns the set of locations that are in bounds right now
    facilityInBounds <- reactive({
        req(input$map_bounds)
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(filteredData(),
               Lat >= latRng[1] & Lat <= latRng[2] &
                   Lon >= lngRng[1] & Lon <= lngRng[2])
    
    })
    
    spillInBounds <- reactive({
        
        req(input$map_bounds)
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(filteredSpillData(),
               Lat >= latRng[1] & Lat <= latRng[2] &
                   Lon >= lngRng[1] & Lon <= lngRng[2])
        
    })
    
    filteredData = reactive({
        storage %>% 
            filter(Material.Family %in% input$material_family_select) %>% 
            filter(Material %in% input$material_name_select) %>% 
            filter(Site.Status %in% input$`site-status`) %>% 
            sample_n(., ifelse(n() < 2e3, n(), 2e3))
        
        
    
    })
    
    filteredSpillData = reactive({
        spills %>% 
            filter(Material.Family %in% input$material_family_select) %>% 
            filter(Material %in% input$material_name_select) %>% 
            filter(Quantity <= 10^input$spill_range[2] & Quantity >= 10^input$spill_range[1]) %>% 
            #filter(`Spill Date` <= input$spill_date[2] & `Spill Date` >= input$spill_date[1]) %>% 
            sample_n(., ifelse(n() < 2e3, n(), 2e3))
        
    })
    ################ County Panel Plots ################ ################ ################ ##########
    
    output$plot_county_spills <- renderPlotly({
        spills %>% 
            filter(DEC.Region == input$dec_select) %>% 
            filter(County == input$county_select) %>%
            filter(Material.Family == input$county_material_family_select) %>% 
            group_by(Material) %>%
            summarise(total_spilled = sum(Quantity)) %>%
            arrange(desc(total_spilled)) %>%
            slice(input$result_range[1]:input$result_range[2]) %>% 
            county_spills()
    })
    
    output$plot_spill_sources <- renderPlotly({
        spills %>% 
            filter(DEC.Region == input$dec_select) %>% 
            filter(County == input$county_select) %>% 
            filter(Material.Family == input$county_material_family_select) %>% 
            group_by(Source_, Material.Family) %>% 
            summarise(total_spilled = sum(Quantity)) %>% 
            filter(log(total_spilled) > 1) %>% 
            arrange(desc(total_spilled)) %>%
            spill_sources()
    })
    
    output$violin <- renderPlotly({
        spills %>% 
            filter(DEC.Region %in% input$dec_select) %>% 
            filter(County %in% input$county_select) %>% 
            filter(Material.Family %in% input$county_material_family_select) %>% 
            group_by(Material.Family %in% input$county_material_family_select) %>% 
            ggplot(aes(log(Quantity))) +
            geom_density(aes(fill=factor(Material.Family)), alpha=0.3, position='fill') +
            #stat_summary(fun.data=mean_sdl, mult=1,geom="pointrange", color="purple") +
            labs(x = "Log of Total Spilled", y='% Total', fill="Material Family")+
            #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
            theme(axis.text.x = element_text(size = 8, angle = 30),
                  legend.position = 'bottom')
        
    })
    
    observeEvent(input$material_family_select, {
        choices = 
            storage %>% 
            filter(Material.Family == input$material_family_select) %>%
            select(Material) %>% 
            unique() 
        updatePickerInput(session = session, inputId = 'material_name_select', choices = sort(choices[[1]]), selected = sort(choices[[1]]))
    })
    
    observeEvent(input$dec_select, {
        choices = 
            spills %>% 
            filter(DEC.Region == input$dec_select) %>%
            select(County) %>% 
            unique() 
        updatePickerInput(session = session, inputId = 'county_select', choices = sort(choices[[1]]), selected = sort(choices[[1]]))
    })
    
    filteredCountyData = reactive({
        spills %>% 
            filter(DEC.Region == input$dec_select) %>%
            filter(County == input$county_select) %>%
            filter(Material.Family == input$county_material_family_select) 
        
        
    })
    output$table <- DT::renderDataTable({
        df = filteredCountyData() %>% select('Spill Number','Facility.Name','Locality',
                                             'Spill Date','Contributing.Factor',
                                             'Source_','Material','Quantity')
        DT::datatable(df, escape = FALSE)
    },
    options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '100px', targets = "_all")))
    )
    
    
    ################ Analysis Panel Plots ################ ################ ################ ##########
    
    observeEvent(input$dec_analysis_select, {
        choices = 
            spills %>% 
            filter(DEC.Region == input$dec_analysis_select) %>%
            select(County) %>% 
            unique() 
        updatePickerInput(session = session, inputId = 'county_analysis_select', choices = sort(choices[[1]]), selected = sort(choices[[1]]))
    })
    
    output$analysis_plot <- renderPlotly({
        time_unit = input$time_unit_select
        spills_byDEC = spills %>% 
            filter(County %in% input$county_analysis_select) %>% 
            filter(Material.Family %in% input$material_family_analysis_select) %>% 
            filter(Source_ %in% input$source_select) %>% 
            mutate(time_unit = lubridate::floor_date(`Spill Date`,tolower(time_unit))) %>% 
            filter(`Report Lag` > 0 & `Case Lag` > 0 & `Report Lag` < 1e3 & `Case Lag` < 1e3) %>% 
            group_by(time_unit,DEC.Region) %>% 
            summarise(Report.Lag = median(`Report Lag`), Case.Lag = median(`Case Lag`), Spills = n()) %>% 
            filter(DEC.Region %in% input$dec_analysis_select) 
        
        g = spills_byDEC %>%
            ggplot(aes(x = time_unit)) +
            geom_area(aes(y = Case.Lag), fill = 'brown', alpha = 0.5) +
            geom_area(aes(y=Spills), fill="purple", alpha=0.7)
        ggplotly(g) %>% layout(height = 700, width = 700)
        
    })
    
    output$offenders_plot <- renderPlotly({
        offenders = storage %>% 
            inner_join(spills, by=c('Facility.Name','Locality','County','DEC.Region')) %>% 
            filter(Material.Family.y %in% input$material_family_analysis_select) %>% 
            filter(DEC.Region %in% input$dec_analysis_select) %>% 
            filter(Source_ %in% input$source_select) %>% 
            filter(Quantity.y > 0) %>% 
            select(Facility.Name,Locality,County,Quantity.x,Quantity.y, `Case Lag`) %>% 
            filter(`Case Lag` > 0 & `Case Lag` < 2e3) %>% 
            group_by(Facility.Name) %>% 
            summarise(Site.Capacity = median(Quantity.x),
                      Spill.Volume = sum(Quantity.y), 
                      Number.of.Spills = n(),
                      Case.Lag = median(`Case Lag`)) %>%
            arrange(desc(Case.Lag)) %>% 
            slice(input$result_range_analysis[1]:input$result_range_analysis[2])
        
        g = offenders %>%
            ggplot(aes(x = reorder(Facility.Name,Spill.Volume)), groups=2) +
            geom_point(aes(y = Spill.Volume,
                           text = paste0(round(Spill.Volume,0)," Gallons Spilled")), color = 'purple') +
            geom_point(aes(y = Site.Capacity,
                       text = paste0(round(Site.Capacity,0)," Gallons of Site Capacity")), color = 'blue') +
            geom_col(aes(y=Case.Lag,
                         text = paste0('Median Time of ',Case.Lag," Days to Close Case")), color = 'black', alpha=0.5) +
            coord_flip() +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            labs(x = '', y = 'Log of Total Volume') +
            theme(axis.ticks.x=element_blank())
        
        ggplotly(g, tooltip = c("text")) %>%
            layout(height = 700, width = 700)
        
    })
    # output$cumgrowth <- renderCachedPlot({
    #     if (input$`show-facilities` == 0)
    #         return()
    #     facilityInBounds() %>% 
    #         group_by(Install.Date) %>% 
    #         summarise(sum_ = sum(Quantity)) %>% 
    #         mutate(cumsum_ = cumsum(sum_)/1e3) %>% 
    #         ggplot(aes(x = Install.Date, y = cumsum_)) +
    #         geom_area(aes(fill="#663399")) +
    #         labs(x='Date',
    #              y='Capacity (kGal)') +
    #         scale_fill_brewer(palette='Set1') +
    #         theme_bw() +
    #         theme(legend.key=element_blank())
    #     
    # },
    # cacheKeyExpr = {
    #     facilityInBounds()
    # })
    # output$histCentile <- renderPlot({
    #     if (input$`show-facilities` == 0)
    #         return()
    #     # If no locations are in view, don't plot
    #     if (nrow(facilityInBounds()) == 0)
    #         return(NULL)
    #     
    #     hist(facilityInBounds()$Quantity,
    #          main = "Material Distribution",
    #          xlab = "Percentile",
    #          xlim = range(storage$Rank),
    #          col = "#663399",
    #          border = 'black')
    # })
    
    # output$topLoc <- renderPlot({
    #     
    #     facilityInBounds() %>% 
    #         # summarise(total = sum(Quantity)) %>%
    #         # top_n(5,total) %>% 
    #         # semi_join(filteredData(), by = 'Site.Type.Name') %>% 
    #         ggplot(aes(x=reorder(Site.Type,Quantity, function(x) sum(x)), y=sum(Quantity))) + 
    #             geom_col(aes(fill=Site.Status)) +
    #             labs(title='Total Capacity by Site',
    #                  x='Sites',
    #                  y='Capacity in Gallons') +
    #             scale_fill_brewer(palette='Set1') +
    #             coord_flip() + 
    #             theme_bw() +
    #             theme(legend.key=element_blank())
    #         
    # })
    
}