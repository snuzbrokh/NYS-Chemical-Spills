library(DT)
library(shiny)
library(leaflet)
library(ggplot2)
library(scales)

mapbox_url = 'https://api.mapbox.com/styles/v1/snuzbrokh/ck9txosrr0ij41iqmmcgby9q7/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic251emJyb2toIiwiYSI6ImNrOXR4Y3V5ejBkYjYzZG96aWlidW11NmsifQ.ZNl03PeSqd0DQwur0AuM3A'
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
data = storage[sample.int(nrow(storage), 10000),]

matdata = data[order(data$Rank),]

function(input, output, session) {
    
    ################ Interactive Map & Functions ################ ################ ################ ######
    output$map <- renderLeaflet({
        leaflet(nys) %>%
            addTiles(urlTemplate = mapbox_url) %>%
            addPolygons(weight=0.5,smoothFactor=0.5,fillOpacity=0.05,
                        highlightOptions=highlightOptions(color = "white",weight = 3,bringToFront = TRUE)) %>% 
            addMeasure() %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Zoom to Level 1",
                onClick=JS("function(btn, map){ map.setZoom(6);}"))) %>%
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="Locate Me",
                onClick=JS("function(btn, map){ map.locate({setView: true});}")))
        })
    
    
    observe({
        leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = ~sqrt(Capacity.in.Gallons)/10, 
                             popup = ~Program.Facility.Name,
                             stroke=FALSE, color = "white",
                             opacity = 1,
                             fillColor = 'green')
    })
    
    # A reactive expression that returns the set of locations that are in bounds right now
    locsInBounds <- reactive({
        req(input$map_bounds)
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(filteredData(),
               Lat >= latRng[1] & Lat <= latRng[2] &
                   Lon >= lngRng[1] & Lon <= lngRng[2])
    })
    
    filteredData = reactive({
        matdata %>% 
            filter(`Material Family` %in% input$material_family_select) %>% 
            filter(Site.Status.Name %in% input$`site-status`) %>% 
            filter(Install.Date < input$plot_date)
        
        
    })
    ################ Interactive Map Plots ################ ################ ################ ##########
    
    output$plot_county_spills <- renderPlotly({
        spills %>% 
            filter(County == input$county_select) %>% 
            group_by(`Material Name`) %>%
            summarise(total_spilled = sum(Quantity)) %>%
            arrange(desc(total_spilled)) %>%
            slice(input$result_range[1]:input$result_range[2]) %>% 
            county_spills()
    })
    
    output$plot_spill_sources <- renderPlotly({
        spills %>% 
            filter(County == input$county_select) %>% 
            group_by(`Source`,`Material Family`) %>% 
            summarise(total_spilled = sum(Quantity)) %>% 
            filter(log(total_spilled) > 1) %>% 
            arrange(desc(total_spilled)) %>%
            spill_sources()
    })
    
    observeEvent(input$material_family_select, {
        choices = 
            storage %>% 
            filter(`Material Family` == input$material_family_select) %>%
            select(Material.Name) %>% 
            unique() 
        updatePickerInput(session = session, inputId = 'materials', choices = sort(choices[[1]]))
    })
    
    # Precalculate the breaks we'll need for the two histograms
    centileBreaks <- hist(plot = FALSE, storage$Rank, breaks = 20)$breaks
    
    output$cumgrowth <- renderPlot({
        
        locsInBounds() %>% 
            group_by(Install.Date) %>% 
            summarise(sum_ = sum(Capacity.in.Gallons)) %>% 
            mutate(cumsum_ = cumsum(sum_)/1e3) %>% 
            ggplot(aes(x = Install.Date, y = cumsum_)) +
            geom_area(aes(fill="#663399")) +
            labs(x='Date',
                 y='Capacity (kGal)') +
            scale_fill_brewer(palette='Set1') +
            theme_bw() +
            theme(legend.key=element_blank())
        
    })
    output$histCentile <- renderPlot({
        # If no locations are in view, don't plot
        if (nrow(locsInBounds()) == 0)
            return(NULL)
        
        hist(locsInBounds()$Capacity.in.Gallons,
             main = "Material Distribution",
             xlab = "Percentile",
             xlim = range(storage$Rank),
             col = "#663399",
             border = 'black')
    })
    
    output$topLoc <- renderPlot({
        
        locsInBounds() %>% 
            # summarise(total = sum(Capacity.in.Gallons)) %>%
            # top_n(5,total) %>% 
            # semi_join(filteredData(), by = 'Site.Type.Name') %>% 
            ggplot(aes(x=reorder(Site.Type.Name,Capacity.in.Gallons, function(x) sum(x)), y=sum(Capacity.in.Gallons))) + 
                geom_col(aes(fill=Site.Status.Name)) +
                labs(title='Total Capacity by Site',
                     x='Sites',
                     y='Capacity in Gallons') +
                scale_fill_brewer(palette='Set1') +
                coord_flip() + 
                theme_bw() +
                theme(legend.key=element_blank())
            
    })

    

    
    ################ Region Plots  ################ ################ ################
    
    
    
    # Observe to Change circle plot of Material
    # observeEvent(input$materials, {
    #     leafletProxy("map", data = filteredData()) %>%
    #         clearShapes() %>%
    #         addCircleMarkers(lat = ~ Lat, lng = ~ Lon, 
    #                          weight = 1, radius = ~(Capacity.in.Gallons)^(1/5), 
    #                          fillOpacity = 0.1)
    # })
    # Cumulative Plot of Material Growth over Time in NYS

    
    # # Barplot of Material by County
    # output$histogram <- renderPlot({
    #     
    #     matdata %>% 
    #         filter(County.Name %in% input$county_select) %>%
    #         ggplot(aes(x=reorder(County.Name, Capacity.in.Gallons, function(x) sum(x)), y=Capacity.in.Gallons)) +
    #         geom_col(aes(fill = Material.Name)) +
    #         labs(title='Total Capacity by County',
    #              x='Site',
    #              y='Capacity in Gallons') +
    #         scale_fill_brewer(palette='Set1') +
    #         coord_flip() +
    #         theme_bw() +
    #         theme(legend.key=element_blank())
    # })
    
    # observe({
    #     colorBy = input$color
    #     sizeBy = input%size
    # })
    # todo Calculate histogram of material by locality and zoom
    # need to convert UTM to lat coordinates first
    # 
    # ## Data Explorer ##############################################
    # observe({
    #     sites <- if (is.null(input$locality)) character(0) else {
    #         storage %>% 
    #             filter(Locality %in% input$locality) %>%
    #             `$`(Site.Type.Name) %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$sites[input$sites %in% sites])
    #     updateSelectizeInput(session, "sites", choices = sites,
    #                          selected = stillSelected, server = TRUE)
    # })
    # 
    # observe({
    #     materials <- if (is.null(input$locality)) character(0) else {
    #         storage %>% 
    #             filter(Locality %in% input$locality) %>%
    #             `$`(Material.Name) %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$sites[input$sites %in% sites])
    #     updateSelectizeInput(session, "materials", choices = materials,
    #                          selected = stillSelected, server = TRUE)
    # })
    # 
    # # observe({
    # #     if (is.null(input$goto))
    # #         return()
    # #     isolate({
    # #         map <- leafletProxy("map")
    # #         map %>% clearPopups()
    # #         dist <- 0.5
    # #         zip <- input$goto$zip
    # #         lat <- input$goto$lat
    # #         lng <- input$goto$lng
    # #         showZipcodePopup(zip, lat, lng)
    # #         map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    # #     })
    # # })
    # 
    # 
    # output$table <- DT::renderDataTable({
    #     df = storage %>% 
    #         filter(
    #             is.null(input$locality) | Locality %in% input$locality,
    #             is.null(input$site) | Site.Type.Name %in% input$site,
    #             is.null(input$material) | Material.Name %in% input$material
    #         )
    #     # %>% mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #     action <- DT::dataTableAjax(session, df, outputId = "table")
    #     
    #     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    #     })
}