library(shiny)
library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Microplastic Density Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map Viewer", tabName = "map", icon = icon("globe")),
      dateInput("date_selector", "Select Date", value = "2024-11-01", format = "yyyy-mm-dd"),
      checkboxInput("show_legend", "Show Legend", value = TRUE),

      tags$style(HTML("
        #download_nc {
          margin-left: 20px;
        }
      ")),
      downloadButton("download_nc", "Download NetCDF", icon = icon("download")),
      submitButton("Update View", icon("refresh"))
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
            leafletOutput("map", height = "600px")
          ),
          box(
            title = "Legend",
            width = 12,
            uiOutput("legend_ui")
          )
        )
      )
    )
  )
)
