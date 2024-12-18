library(shiny)
library(leaflet)
library(terra)
library(ncdf4)
library(leaflet.extras)

server <- function(input, output, session) {
  
  # đọc dữ liệu
  observeEvent(input$date_selector, {
    date_selected <- as.character(input$date_selector)
    
    # tạo tên file thoe ngày tháng đinh dạng yyyymmdd
    date_formatted <- gsub("-", "", date_selected)
    
    # duyệt tệp và tìm ngày
    file_list <- list.files("ncdata", pattern = paste0("cyg.ddmi.s", date_formatted), full.names = TRUE)
    
    # kiểm tra tệp
    if (length(file_list) == 0) {
      showNotification("No data available for the selected date.", type = "error")
      return()
    }
    
    # lấy tệp đầu tiên nếu có nhiều tệp giống nhau
    file_path <- file_list[1]
    
    # đọc dữ liệu tệp nc
    r <- rast(file_path)
    df <- as.data.frame(r, xy = TRUE)
    names(df) <- c("longitude", "latitude", "mp_concentration")
    
    # render bản đồ
    output$map <- renderLeaflet({
      
      # heatmap
      map <- leaflet(data = df, options = leafletOptions(minZoom = 1.2, maxZoom = NULL)) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addHeatmap(
          lng = ~longitude, lat = ~latitude, intensity = ~mp_concentration, 
          blur = 60, max = 0.05, radius = 13, 
          gradient = c("lightblue", "yellow", "green", "orange", "red")
        ) %>%
        setView(
          lng = mean(df$longitude, na.rm = TRUE), 
          lat = mean(df$latitude, na.rm = TRUE), 
          zoom = 2.5
        ) %>%
        setMaxBounds(lng1 = -180, lat1 = NULL, lng2 = 180, lat2 = NULL)
      
      # thanh legend
      if (input$show_legend) {
        map <- map %>%
          addLegend(
            pal = colorNumeric(
              palette = c("lightblue", "yellow", "green", "orange", "red"),
              domain = df$mp_concentration,
              na.color = "transparent"
            ),
            values = df$mp_concentration,
            title = "Microplastic Concentration",
            position = "bottomright"
          )
      }
      
      map
    })
    
  })
  
  # dowload tệp nc
  output$download_nc <- downloadHandler(
    filename = function() {
      # tạo tên file nc
      date_selected <- as.character(input$date_selector)
      date_formatted <- gsub("-", "", date_selected)  # Chuyển đổi "2024-11-01" thành "20241101"
      
      # duyệt và tìm file 
      file_list <- list.files("ncdata", pattern = paste0("cyg.ddmi.s", date_formatted), full.names = TRUE)
      
      # kiểm tra tệp
      if (length(file_list) == 0) {
        stop("No data available for the selected date.")
      }
      
      # lấy tệp đầu tiên nếu nhiều tệp trùng
      file_path <- file_list[1]
      
      # lấy tên file gốc cho tệp dowload
      return(basename(file_path))
    },
    content = function(file) {
      
      # tạo tên tệp
      date_selected <- as.character(input$date_selector)
      date_formatted <- gsub("-", "", date_selected)  # Chuyển đổi "2024-11-01" thành "20241101"
      
      # duyệt và tìm tên tệp
      file_list <- list.files("ncdata", pattern = paste0("cyg.ddmi.s", date_formatted), full.names = TRUE)
      
      # kiểm tra
      if (length(file_list) == 0) {
        stop("No data available for the selected date.")
      }
      
      # lấy tệp đầu tiên
      file_path <- file_list[1]
      
      # sao chép tên tệp để chuẩn bị tải xuống
      file.copy(file_path, file)
    }
  )
}
