library(shiny)

# Gọi UI và Server từ file
source("ui.R")
source("server.R")

# Chạy ứng dụng
shinyApp(ui, server)

