library(shiny)
library(shinyjs)
library(ggplot2)
library(leaflet)
library(googleway)
library(showtext)

# Cargar fuente
#font_add_google("Roboto", "roboto")
sysfonts::font_add_google("Roboto", "roboto")
showtext_auto()

source("aux_functions.R")

# Paleta de colores
blue_palette <- c("#dbe9f6", "#a6c9e2", "#6ea6d2", "#3d85c6", "#145a9e")

ui <- fluidPage(
  useShinyjs(),  # Habilitar shinyjs para controlar la visibilidad
  tags$head(
    tags$style(HTML("
            body {
                font-family: 'Roboto', sans-serif;
                background-color: #f5f5f5;
            }
            .container {
                width: 80%;
                max-width: 805px;
                background-color: #fff;
                border-radius: 8px;
                box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                padding: 20px;
                display: flex;
                flex-direction: column;
                align-items: center;
                margin: 0 auto;
            }
            .search-bar {
                display: flex;
                justify-content: center;
                margin-bottom: 20px;
            }
            #search-input {
                width: 60%;
                padding: 10px;
                border: 1px solid #ccc;
                border-radius: 4px;
            }
            #search-button, #advanced-search-button {
                padding: 10px;
                border: none;
                color: white;
                cursor: pointer;
                border-radius: 4px;
                margin-left: 10px;
            }
            #search-button {
                background-color: #3d85c6;
            }
            #advanced-search-button {
                background-color: #a6c9e2;
            }
            .advanced-filters {
                display: none; /* Oculto por defecto */
                margin: 10px 0;
                width: 100%;
            }
            .advanced-filters label {
                display: block;
                margin-top: 10px;
            }
            .visualization-container {
                display: flex;
                flex-direction: column;
                width: 100%;
                margin-top: 20px;
            }
            #map {
                height: 300px;
                background-color: #eaeaea; /* Placeholder para el mapa */
                margin-bottom: 20px;
            }
            #species-image {
                height: 200px;
                background-color: #dbe9f6; /* Placeholder para la imagen de la especie */
                margin-bottom: 20px;
            }
            #histogram,
            #time-series {
                height: 300px;
                background-color: #f0f0f0; /* Placeholder para gráficos */
                margin-bottom: 20px;
            }
        "))
  ),
  
  # Estructura principal
  div(class = "container",
      div(class = "search-bar",
          textInput("search_input", "Buscar por nombre científico o vernáculo...", ""),
          actionButton("search_button", "Buscar", icon = icon("search")),
          actionButton("advanced_search_button", "Búsqueda Avanzada")
      ),
      
      div(class = "advanced-filters", id = "advanced-filters",
          textInput("family", "Familia:", ""),
          textInput("kingdom", "Reino:", ""),
          textInput("region", "Región:", ""),
          selectInput("month", "Mes del año:", 
                      choices = month.abb,
                      selected = NULL),
          textInput("hour", "Hora:", "")
      ),
      
      selectInput("species_select", "Seleccione una especie:", choices = NULL),
      
      div(class = "visualization-container",
          leafletOutput("map"),
          div(id = "species_image"),
          div(id = "histogram"),
          div(id = "time_series")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$search_button, {
    # Aquí puedes manejar la lógica de búsqueda
    query <- input$search_input
    cat("Buscando:", query, "\n")
    # Lógica para buscar la especie
    # Aquí puedes agregar código para actualizar las opciones de especies en el selectInput
  })
  
  observeEvent(input$advanced_search_button, {
    toggle("advanced-filters")  # Alternar la visibilidad de los filtros avanzados
  })
  
  
  
  output$map <- renderLeaflet({timeline_map1})
  
}

shinyApp(ui, server)



