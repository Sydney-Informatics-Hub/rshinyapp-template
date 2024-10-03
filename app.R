# Load dependencies
library(dplyr)          # For data manipulation
library(here)           # For file path management
library(leaflet)        # For interactive maps
library(shiny)          # For web application framework
library(shinythemes)    # For Shiny themes
library(shinyWidgets)   # For additional Shiny UI components
library(tidyverse)      # For data manipulation and visualization
library(viridis)        # For color palettes
library(shinyjs)        # For JavaScript operations in Shiny
library(sf)             # For spatial data handling
library(ozmaps)         # For Australian map data
library(rnaturalearth)  # For NSW cities
library(rnaturalearthdata) # For NSW cities
library(markdown)

# Load and prepare data
nsw_sheep_geometries <- qs::qread(here::here("nsw_sheep_geometries_clean.qs")) #new format
nsw_cities <- qs::qread(here::here("nsw_cities.qs")) #extracted label for towns and cities

# Define UI
ui <- fluidPage(
  # Add custom CSS for italics
  tags$head(
    tags$style(HTML("
      #species_selector .radio label {
        font-style: italic;
      }
      #species_selector .radio label .snp-part, #snp_selector .radio label .snp-part {
        font-style: normal;
      }
      .small-select .selectize-input {
      font-size: 12px;
      padding: 2px 2px;
      }
      .small-select .selectize-dropdown {
      font-size: 12px;
      }
    .small-select .item {
      padding: 2px 2px !important;
      }
    "))
  ),
  # Set theme and enable shinyjs
  theme = shinytheme("flatly"),
  useShinyjs(),
  # Application title
  titlePanel(h3(HTML("<b>Wo</b>rm<b>R</b>esistance<b>M</b>onitor (<b>WoRM</b>) Dashboard")), windowTitle = "WoRM Dashboard"),
  fluidRow(
    # Sidebar with input controls
    column(3,
           style = "padding-right: 0;",
           wellPanel(
             style = "height: 850px; overflow-y: auto; font-size: 14px;",
             # Host selection
             selectInput("host", label = h5(HTML("<b>Select host:</b>")),
                         choices = c("Sheep" = "ovine", "Cow" = "bovine", "Goat" = "caprine", "Alpaca" = "alpaca"),
                         selected = "ovine"),
             # Gene marker selection
             radioButtons("marker", label = h5(HTML("<b>Select gene marker:</b>")),
                          choices = c("Nematode species" = "its2",
                                      "Benzimidazole resistance" = "bz",
                                      "Levamisole resistance" = "lev")),
             # Group by option
             selectInput("group_by_option", label = h5(HTML("<b>Group by:</b>")),
                         choices = c("Postcode" = "postcode", "Region" = "worm_trax_region"),
                         selected = "postcode"),
             # Percentage display option
             radioButtons("perc_option", label = h5(HTML("<b>Select percentage to display:</b>")),
                          choices = c("All" = "all", "5% and above" = "5_plus", "16% and above" = "16_plus")),
             # Sampling season selection
             div(class = "small-select",
                 selectizeInput("season_of_sampling",
                                label = h5(HTML("<b>Select season(s) of sampling:</b>")),
                                choices = unique(nsw_sheep_geometries$season_of_sampling),
                                multiple = TRUE)
             ),
             # Sampling year selection
             div(class = "small-select",
                 selectizeInput("year_of_sampling",
                                label = h5(HTML("<b>Select year(s) of sampling:</b>")),
                                choices = unique(year(nsw_sheep_geometries$date_of_sampling)),
                                multiple = TRUE)
             ),
             tags$br(),
             # Include additional markdown content here
             tags$img(src = "worm_logo_edit.png", height = 30),
             includeMarkdown("include.md"),
             #tags$br(),
             # Display logos, need to add DPI and Vet Faculty
             tags$img(src = "sih_logo.png", height = 40, style = "padding: 2px;"),
             tags$img(src = "vet_logo_edit.png", height = 40, style = "padding: 2px;"),
             tags$img(src = "dpi_logo.png", height = 40, style = "padding: 2px;"),
           )
    ),
    # Main panel with map and read me, not sure if we want to keep region tab
    column(9,
           style = "padding-left: 0;",
           tabsetPanel(id="main_tabs", type = "tabs",
                       tabPanel("Map",
                                div(style = "position: relative;",
                                    leafletOutput("map", height = 800),
                                    # Overlay panels for additional controls
                                    absolutePanel(
                                      top = 10, right = 10,
                                      # Aggregation method selection
                                      div(id = "aggregation_panel",
                                          style = "background-color: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px; margin-bottom: 5px; width: 150px;",
                                          selectInput("aggregation_method", "Aggregation method",
                                                      choices = c("Min" = "min", "Median" = "median", "Mean" = "mean", "Max" = "max"),
                                                      selected = "mean", width = "160px")
                                      ),
                                      # Species selection
                                      div(id = "species_panel",
                                          style = "background-color: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px; max-height: auto; overflow-y: auto; width: 150px; margin-bottom: 10px;",
                                          uiOutput("species_selector")
                                      ),
                                      # SNP selection (conditional on marker - only BZ or LEV)
                                      conditionalPanel(
                                        condition = "input.marker == 'bz' || input.marker == 'lev'",
                                        div(style = "height: 5px;"),
                                        div(id = "snp_panel",
                                            style = "background-color: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px; max-height: auto; overflow-y: auto; width: 150px;",
                                            # This helps maintain its state and prevents flickering
                                            uiOutput("snp_selector", suspendWhenHidden = FALSE)
                                        )
                                      )
                                    )
                                )
                       ),
                       # About tab
                       tabPanel("About",
                                div(
                                  style = "height: 850px; overflow-y: auto; padding: 30px;",
                                  includeMarkdown("about.md")
                                ),
                       tags$script(HTML("document.addEventListener('DOMContentLoaded', function() {
                       document.querySelector('a[href=\"#read-me\"]').addEventListener('click', function(e) {
                       e.preventDefault();
                       Shiny.setInputValue('read_me', Math.random());
                       });
                       });
                                        ")))
           )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  #handle tab switching
  observeEvent(input$read_me, {
    updateTabsetPanel(session, "main_tabs", selected = "About")
  })

  # Initialize inputs with default values
  observe({
    updateRadioButtons(session, "host", selected = "ovine")
    updateRadioButtons(session, "marker", selected = "its2")
    updateRadioButtons(session, "perc_option", selected = "all")
    updateSelectizeInput(session, "season_of_sampling", selected = unique(nsw_sheep_geometries$season_of_sampling))
    updateSelectizeInput(session, "year_of_sampling", selected = unique(year(nsw_sheep_geometries$date_of_sampling)))
  })

  # Reactive function to filter data based on user input
  filtered_data <- reactive({
    req(input$host, input$season_of_sampling, input$year_of_sampling, input$marker)
    nsw_sheep_geometries %>%
      filter(host %in% input$host,
             season_of_sampling %in% input$season_of_sampling,
             year(date_of_sampling) %in% input$year_of_sampling,
             marker == input$marker)
  })

  # This helps prevent rapid changes from causing issues when switching markers
  marker_debounced <- reactive(input$marker) %>% debounce(200)

  # initialize the reactive values for species and SNP mappings because we format them a lot
  rv <- reactiveValues(
    species_mapping = NULL,
    snp_mapping = NULL,
    show_snp_selector = FALSE
  )

  # Dynamic UI for species selection
  output$species_selector <- renderUI({
    req(filtered_data())

    species_choices <- unique(filtered_data()$species_clean)
    species_names <- setNames(species_choices, species_choices)
    rv$species_mapping <- species_names

    radioButtons("selected_species", label = h5(HTML("<b>Select species:</b>")),
                 choices = species_names,
                 selected = ifelse("H. contortus" %in% species_choices,
                                   "H. contortus", species_choices[1]))
  })

  # Dynamic UI for SNP selection
  output$snp_selector <- renderUI({
    req(filtered_data(), input$selected_species, rv$show_snp_selector)

    if(!rv$show_snp_selector) return(NULL)

    snp_choices <- filtered_data() %>%
      filter(species_clean == input$selected_species, !is.na(variant)) %>%
      pull(variant) %>%
      unique()

    if(length(snp_choices) == 0) return(NULL)

    rv$snp_mapping <- setNames(paste0(snp_choices), snp_choices)

    pre_selected <- if (input$marker == "bz") {
      snp_choices[str_detect(snp_choices, "f200y")][1]
    } else {
      snp_choices[str_detect(snp_choices, "s168t|s140t")][1]
    }

    radioButtons("selected_snps", label = h5(HTML("<b>Select SNP:</b>")),
                 choices = setNames(snp_choices, rv$snp_mapping),
                 selected = pre_selected)
  })

  # This helps manage the visibility of the SNP selector more smoothly
  observe({
    rv$show_snp_selector <- marker_debounced() %in% c("bz", "lev")
  })

  # Reactive expression for selected columns
  selected_rows <- reactive({
    req(input$selected_species)
    if(input$marker %in% c("bz", "lev") && !is.null(input$selected_snps)) {
      list(species = input$selected_species, variant = input$selected_snps)
    } else {
      list(species = input$selected_species)
    }
  })

  # Data aggregation
  aggregated_data <- reactive({
    req(input$marker, input$perc_option, selected_rows(), input$aggregation_method, input$season_of_sampling, input$selected_species)

    data <- filtered_data()
    rows <- selected_rows()

    # Safe aggregation function
    safe_aggregation <- function(x, method) {
      if (all(is.na(x))) return(NA)
      switch(method,
             "mean" = mean(x, na.rm = TRUE),
             "median" = median(x, na.rm = TRUE),
             "min" = min(x, na.rm = TRUE),
             "max" = max(x, na.rm = TRUE))
    }

    # Determine grouping column based on selected option
    group_column <- input$group_by_option

    # Aggregate data
    data <- data %>%
      filter(species_clean == rows$species,
             season_of_sampling %in% input$season_of_sampling) %>%
      {if (!is.null(rows$variant)) filter(., variant == rows$variant) else .} %>%
      group_by(across(all_of(group_column))) %>%
      summarise(
        value = safe_aggregation(percentage, input$aggregation_method),
        reads = safe_aggregation(reads, input$aggregation_method),
        total_reads = safe_aggregation(total_reads, input$aggregation_method),
        sample_size = n(),
        total_samples = n(),
        geometry = if (group_column == "worm_trax_region") {
          st_union(geometry)  # Aggregate geometries for regions
        } else {
          first(geometry)  # Use first geometry for postcodes
        }
      ) %>%
      ungroup()

    # Apply threshold if not showing all percentages
    if (input$perc_option != "all") {
      threshold <- if (input$perc_option == "5_plus") 5 else 16

      data <- data %>%
        mutate(value_threshold = case_when(
          is.na(value) ~ NA_character_,
          value >= threshold ~ "Above threshold",
          TRUE ~ "Below threshold"
        ))
    }

    data
  })

  # Get NSW border
  nsw_border <- reactive({
    ozmap_states %>%
      filter(NAME == "New South Wales") %>%
      sf::st_transform(4326)
  })

  # Create the initial map
  output$map <- renderLeaflet({

    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satellite Image") %>%
      addTiles("https://tiles.stadiamaps.com/tiles/stamen_toner_lines/{z}/{x}/{y}{r}.png",
               options = tileOptions(opacity = 0.8), group = "Roads") %>%
      addPolylines(data = nsw_border(), weight = 1, color = "black", opacity = 1, group = "nsw_border") %>%
      setView(lng = 146.9211, lat = -32.2016, zoom = 6)
  })

  # Render Leaflet map
  observe({
    req(input$marker, input$perc_option, input$selected_species)

    data <- aggregated_data()

    # Set up color palette and legend based on percentage option
    if (input$perc_option == "all") {
      pal <- colorNumeric("viridis", domain = c(0, 100), na.color = "transparent")
      legend_values <- seq(0, 100, by = 20)
      fill_color <- pal(pmin(pmax(data$value, 0, na.rm = TRUE), 100, na.rm = TRUE))
      legend_title <- if(input$marker == "its2") "% of total reads" else "% of species reads"
    } else {
      threshold_colors <- c("Below threshold" = "#D55E00", "Above threshold" = "#0072B2")
      pal <- colorFactor(threshold_colors, domain = c("Below threshold", "Above threshold"), na.color = "transparent")
      legend_values <- c("Below threshold", "Above threshold")
      fill_color <- pal(data$value_threshold)
      legend_title <- "Scenarios"
    }

    # Create popup content
    popup_content <- ~sapply(seq_len(nrow(data)), function(i) {
      # Create a mapping for friendly display names
      display_name_mapping <- list(
        postcode = "Postcode",
        worm_trax_region = "Region"
      )

      display_name <- if (input$group_by_option %in% names(display_name_mapping)) {
        display_name_mapping[[input$group_by_option]]
      } else {
        input$group_by_option
      }

      content <- paste0("<strong>", display_name, ":</strong> ", data[[input$group_by_option]][i],
                        " (n=", data$total_samples[i], ")<br>")

      name <- if (input$marker %in% c("bz", "lev") && !is.null(input$selected_snps)) {
        gsub("(.*) (\\(.*\\))", "<em>\\1</em> \\2", rv$snp_mapping[input$selected_snps])
      } else {
        paste0("<em>", input$selected_species, "</em>")
      }

      if (input$perc_option == "all") {
        value <- sprintf("%.2f%% (%s out of %s reads)",
                         data$value[i],
                         format(round(data$reads[i]), nsmall = 0),
                         format(round(data$total_reads[i]), nsmall = 0))
      } else {
        threshold_value <- data$value_threshold[i]
        actual_value <- sprintf("%.2f%% (%s out of %s reads)",
                                data$value[i],
                                format(round(data$reads[i]), nsmall = 0),
                                format(round(data$total_reads[i]), nsmall = 0))
        value <- paste0(threshold_value, " (", actual_value, ")")
      }

      paste0(content, "<strong>", name, ":</strong> ", value)
    })

    leafletProxy("map") %>%
      clearGroup("data_polygons") %>%
      clearGroup("city_labels") %>%
      clearControls() %>%
      addPolylines(data = nsw_border(), weight = 1, color = "black", opacity = 1) %>%
      addPolygons(data = data, fillColor = fill_color, fillOpacity = 0.7,
                  color = "black", weight = 1, popup = popup_content,
                  highlightOptions = highlightOptions(
                    weight = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = ~paste("Click for more info"),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto"),
                  group = "data_polygons") %>%
      addLegend("bottomright", pal = pal, values = legend_values,
                title = legend_title, opacity = 1,
                labFormat = ifelse(input$perc_option == "all",
                                   labelFormat(suffix = "%"),
                                   labelFormat(prefix = ""))) %>%
      addLabelOnlyMarkers(
        data = nsw_cities,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        label = ~NAME,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'top',
          textOnly = TRUE,
          style = list(
            "font-size" = "8px",
            "font-weight" = "regular",
            "color" = "white"
          )
        ),
        group = "city_labels"
      )
  })

  observeEvent({input$map_zoom; input$map_center}, {
    rv$mapZoom <- input$map_zoom
    rv$mapCenter <- input$map_center
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)
