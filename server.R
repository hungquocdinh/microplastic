  library(shiny)
  library(leaflet)
  library(terra)
  library(leaflet.extras)
  
  server <- function(input, output, session) {
    # Dynamically update the slider when the date range is changed
    observeEvent(input$date_selector, {
      output$rangeSelector <- renderUI({
        lab <- seq(
          from = input$date_selector[1],
          to = input$date_selector[2],
          by = 1
        )
        sliderInput(
          "layer",
          "Select Raster Layer No.:",
          min = 1,
          max = length(lab),
          value = 1,
          step = 1,
          animate = TRUE
        )
      })
    })
    
    # Reactive to load data based on selected date range
    data <- eventReactive(input$apply_btn, {
      lab <- seq(
        from = input$date_selector[1],
        to = input$date_selector[2],
        by = 1
      )
      showNotification("Loading data", type = "message")
      file_list <- sapply(
        lab,
        pocloud.download,
        base.url = "https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2893924134-POCLOUD/temporal",
        base.download.url = "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/",
        token = Sys.getenv("EARTH_DATA_BEARER_TOKEN"),
        destination = "./data/"
      )
      
      if (any(is.na(file_list))) {
        showNotification("Data file is not found", type = "error")
        return(NULL)
      }
      # Read and reproject data
      data <- lapply(file_list, rast, subds = 1)
      reprojected_raster <- lapply(data, project, "EPSG:3857")
      showNotification("Loading data: DONE!", type = "message")
      return(reprojected_raster)
    })
    
    # Prepare the map when the date range changes
    observeEvent(input$apply_btn, {
      showNotification("Preparing map", type = "message")
      
      list.raster <- data()
      if (is.null(list.raster))
        return()  # Exit if no data is loaded
      
      color_pal <- colorNumeric(
        palette = "viridis",
        domain = c(1000, 30000),
        na.color = "transparent"
      )
      
      # Initialize map
      leaflet_proxy <- leafletProxy("raster_map") %>%
        clearShapes() %>%  # Clear previous layers
        addProviderTiles(providers$Esri.WorldTopoMap) 
      
      # Add rasters as layers
      for (index in seq_along(list.raster)) {
        r <- list.raster[[index]]
        leaflet_proxy <- addRasterImage(
          leaflet_proxy,
          r,
          opacity = 0.9,
          colors = color_pal,
          group = as.character(index)  # Use group names matching indices
        )
      }
      
      showNotification("Preparing map: DONE!", type = "message")
    })
    
    # Toggle layers when the slider changes
    observeEvent(input$layer, {
      showNotification("Toggling layer visibility", type = "message")
      list.raster <- data()
      if (is.null(list.raster))
        return()  # Exit if no data is loaded
      
      layer.max <- length(list.raster)
      groups <- as.character(seq_len(layer.max))
      selected_layer <- as.character(input$layer)
      
      hide_groups = group[group > selected_layer]
      
      leaflet_proxy <- leafletProxy("raster_map")
      leaflet_proxy %>%
        hideGroup(hide_groups) %>%  # Hide all groups
        showGroup(selected_layer)  # Show the selected layer
    })
    
    # Initialize map output
    output$raster_map <- renderLeaflet({
      leaflet() %>%
        setView(lng = 0,
                lat = 0,
                zoom = 4) %>%
        addLegend(
          pal = color_pal,
          values = c(1000, 30000),
          title = "Raster Values",
          position = "bottomright"
        )
    })
    
    observeEvent(input$raster_map_click, {
      showNotification("Clicked ", type = "message")
      click <- input$raster_map_click
      if (!is.null(click)) {
        coords <- paste0("Click at: Longitude: ", click$lng, "\nLatitude: ", click$lat)
        showNotification(coords, type = "message")
        output$plot <- renderPlot({ 
          data = data()
          val = sapply(data, extract, cbind(click$lng, click$lat))
          time = sapply(data, time)
          leaflet_proxy <- leafletProxy("raster_map") %>% clearMarkers() %>% addMarkers(lng = click$lng, lat= click$lat, popup = coords)
          plot(time, val, type = "b")
          })
      }
    })
    
    
  }
