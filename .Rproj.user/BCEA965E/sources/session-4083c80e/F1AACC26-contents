library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(readr)

# Sample dataset for Ethiopia (replace with real data)
nutrition_data <- data.frame(
  Region = c("Tigray", "Afar", "Amhara", "Oromia", "Somali", "Benishangul-Gumuz", "SNNPR", "Gambela", "Harari", "Addis Ababa", "Dire Dawa"),
  Stunting = c(35, 40, 32, 30, 45, 38, 33, 41, 29, 20, 25),
  Wasting = c(10, 12, 8, 7, 15, 11, 9, 13, 6, 5, 7),
  Dietary_Diversity = c(50, 55, 60, 62, 48, 52, 58, 53, 65, 75, 70)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Ethiopia Nutrition M&E Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Map", tabName = "map", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(plotlyOutput("stunting_plot"), width = 6),
                box(plotlyOutput("wasting_plot"), width = 6)
              )
      ),
      tabItem(tabName = "table", DTOutput("data_table")),
      tabItem(tabName = "map", leafletOutput("map"))
    )
  )
)

# Server
server <- function(input, output) {
  output$stunting_plot <- renderPlotly({
    p <- ggplot(nutrition_data, aes(x = Region, y = Stunting, fill = Region)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Stunting Rates by Region")
    ggplotly(p)
  })
  
  output$wasting_plot <- renderPlotly({
    p <- ggplot(nutrition_data, aes(x = Region, y = Wasting, fill = Region)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Wasting Rates by Region")
    ggplotly(p)
  })
  
  output$data_table <- renderDT({
    datatable(nutrition_data, options = list(pageLength = 5))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = c(39.5, 40.6, 37.5, 38.5, 42.0, 34.5, 37.0, 34.3, 42.1, 38.75, 41.86),
                 lat = c(13.5, 11.5, 11.5, 7.5, 6.0, 10.5, 6.5, 7.8, 9.3, 9.03, 9.6),
                 popup = nutrition_data$Region)
  })
}

# Run App
shinyApp(ui, server)
