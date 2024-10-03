# #renv
# renv::init()
# renv::snapshot()

# Load dependencies
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(qs)
library(markdown)

# Load dummy data with geometries
dummy_data <- qread("input_file.qs")

# Ensure dummy_data is an sf object
if (!inherits(dummy_data, "sf")) {
  dummy_data <- st_make_valid(dummy_data)
}

# Define UI
ui <- fluidPage(
  titlePanel("Simplified Map Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("letter", "Select Letter:", 
                  choices = unique(dummy_data$letters)),
      selectInput("aggregation_method", "Aggregation Method:",
                  choices = c("Mean" = "mean", "Median" = "median", 
                              "Min" = "min", "Max" = "max"),
                  selected = "mean"),
      tags$br(),
      h5("More Information:"),
      includeMarkdown("include.md"),
      tags$br(),
      tags$img(src = "sih_logo.png", height = 60),
      tags$br(),
      h5("Debugging Output:"),
      verbatimTextOutput("debug")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for filtered and aggregated data
  aggregated_data <- reactive({
    req(input$letter, input$aggregation_method)
    
    data <- dummy_data %>%
      filter(letters == input$letter) %>%
      group_by(poa_code21, color) %>%
      summarise(
        value = switch(input$aggregation_method,
                       "mean" = mean(number, na.rm = TRUE),
                       "median" = median(number, na.rm = TRUE),
                       "min" = min(number, na.rm = TRUE),
                       "max" = max(number, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Ensure the result is still an sf object
    if (!inherits(data, "sf")) {
      data <- left_join(data, dummy_data %>% select(poa_code21, geometry) %>% distinct(), by = "poa_code21")
      data <- st_as_sf(data)
    }
    
    # Ensure valid geometries and remove any that can't be fixed
    data <- st_make_valid(data)
    data <- data[st_is_valid(data), ]
    
    # Debug output
    output$debug <- renderPrint({
      data <- aggregated_data()
      cat("Class of data:", class(data), "\n")
      cat("Number of rows:", nrow(data), "\n")
      cat("Columns:", paste(names(data), collapse = ", "), "\n")
      cat("Is geometry valid:", all(st_is_valid(data)), "\n")
      cat("Unique letters in original data:", paste(unique(dummy_data$letters), collapse = ", "), "\n")
      cat("Selected letter:", input$letter, "\n")
      cat("Number of rows for selected letter:", nrow(dummy_data[dummy_data$letters == input$letter,]), "\n")
      cat("Number of valid geometries:", sum(st_is_valid(data)), "\n")
    })
    
    data
  })
  
  # Render map
  output$map <- renderLeaflet({
    data <- aggregated_data()
    
    # Check if data is empty
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 151.2093, lat = -33.8688, zoom = 10))
    }
    
    pal <- colorNumeric(palette = "viridis", domain = data$value)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = data, 
                  fillColor = ~pal(value),
                  fillOpacity = 0.7,
                  color = "white",
                  weight = 1,
                  popup = ~paste("Postcode:", poa_code21, "<br>",
                                 "Value:", round(value, 2))) %>%
      addLegend("bottomright", pal = pal, values = data$value,
                title = "Value",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)