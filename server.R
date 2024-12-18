library(shiny)
library(leaflet)
library(terra)
library(ncdf4)
library(leaflet.extras)

source("lib/POCLOUD.R")

server <- function(input, output, session) {
  data = reactive({
    file_list <- pocloud.download(date = input$date_selector)
    if (length(file_list) == 0) {
      showNotification("No data available for the selected date.", type = "error")
      return()
    }
    
    # lấy tệp đầu tiên nếu có nhiều tệp giống nhau
    nc4.filename <- file_list[1]
    print(nc4.filename)
    
    # đọc dữ liệu tệp nc
    data = rast(nc4.filename, subds = 1)
    reprojected_raster <- project(data, "EPSG:3857")
  })
  
  output$map <- renderLeaflet({
    reprojected_raster = data()
    color_pal <- colorNumeric(
      palette = "viridis",
      domain = values(reprojected_raster),
      na.color = "transparent"
    )
    map = leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(reprojected_raster,
                     colors = color_pal,
                     opacity = 0.9) %>%
      addLegend(
        pal = color_pal,
        values = values(reprojected_raster),
        title = "Raster Values",
        position = "bottomright"
      )
    
    map
    
  })
}
