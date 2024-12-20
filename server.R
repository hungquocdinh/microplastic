library(shiny)
library(leaflet)
library(terra)
library(ncdf4)
library(leaflet.extras)

source("lib/POCLOUD.R")

server <- function(input, output, session) {
  #update animated
  observeEvent(input$date_selector, {
    output$rangeSelector <- renderUI({
      lab = seq(
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
  
  
  
  data = reactive({
    lab = seq(
      from = input$date_selector[1],
      to = input$date_selector[2],
      by = 1
    )
    showNotification("Loading data", type = "message")
    print(lab)
    file_list = sapply(
      lab,
      pocloud.download,
      base.url = "https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2893924134-POCLOUD/temporal",
      base.download.url = "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/",
      token = Sys.getenv("EARTH_DATA_BEARER_TOKEN"),
      destination = "./data/"
    )
    
    if (any(is.na(file_list))) {
      showNotification("Data file is not found", type = "error")
      return(NA)
    }
    # đọc dữ liệu tệp nc
    data = lapply(file_list, rast, subds = 1)
    reprojected_raster <- lapply(data, project, "EPSG:3857")
    showNotification("Loading data: DONE!", type = "message")
    return(reprojected_raster)
  })
  
  reactiveRaster <- reactive({
    d = data()
    if (any(is.na(d))) {
      return(NA)
    }
    return (d[[input$layer]])
  })
  
  output$raster_map <- renderLeaflet({
    color_pal <- colorNumeric(palette = "viridis",
                              domain = c(10000,25000),
                              na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(0, 0, zoom = 4) %>% 
      addLegend(
        pal = color_pal,
        values =c(10000,25000),
        title = "Raster Values",
        position = "bottomright"
      )
  })
  
  observeEvent(input$layer, {
    print("change layer")
    r = reactiveRaster()
    if (any(is.logical(r))) {
      return(NA)
    }
    
    map = leafletProxy("raster_map") 
    layer_max = length(seq(
      from = input$date_selector[1],
      to = input$date_selector[2],
      by = 1
    ))
    id2clr = seq(input$layer,layer_max, by = 1)
    for (id in id2clr) {
      removeImage(map, id)
    }
    map %>%
      addRasterImage(
        r,
        opacity = 0.9,
        colors = color_pal,
        layerId = input$layer
        ) 
  })
}
