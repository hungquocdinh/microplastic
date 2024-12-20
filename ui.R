library(shiny)
library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Microplastic Density Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map Viewer", tabName = "map", icon = icon("globe")),
      dateRangeInput("date_selector", "Select Date", format = "yyyy-mm-dd", start = "2024-11-01", end = "2024-11-03"),
      uiOutput("rangeSelector"),
      downloadButton("download_nc", "Download NetCDF", icon = icon("download")),
      actionButton("apply_btn", "Apply", icon("refresh"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Microplastic Concentration Map",
            width = 12,
            leafletOutput("raster_map", height = "600px")
          ),
          box(
            title = "Plot",
            width = 12,
            plotOutput("plot")
          )
        )
      )
    )
  )
)
