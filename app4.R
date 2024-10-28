library(shiny)
library(leaflet)
library(plotly)
library(bslib)
library(dplyr)
library(geosphere)
library(lubridate)


biodata <- read.csv("data/poland_data_sample.csv")
pics_biodata <- read.csv("data/multimedia_poland.csv")

source("assets/graphs.R")

poland_regions <- data.frame(
  region = c("Greater Poland", "Lesser Poland", "Masovian", "Silesian", "Pomeranian", "West Pomeranian"),
  lat = c(52.4064, 50.0647, 52.2297, 50.2945, 54.3520, 53.4285),
  lon = c(16.9252, 19.9450, 21.0122, 19.0224, 18.6466, 14.5528)
)

biodata <- biodata %>%
  # filter(scientificName %in% c("Lanius collurio",
  #                              "Grus grus ",
  #                              "Emberiza citrinella",
  #                              "Ciconia ciconia" ,
  #                              "Carpodacus erythrinus")) %>% 
  left_join(pics_biodata %>% select(CoreId,accessURI), by = c("id" = "CoreId")) %>%
  mutate(accessURI = ifelse(is.na(accessURI),
                            "https://upload.wikimedia.org/wikipedia/commons/1/14/No_Image_Available.jpg",
                            accessURI)) %>% 
  mutate(month = month(eventDate, label = TRUE, abbr = FALSE)) 


biodata <- biodata %>%
  rowwise() %>%
  mutate(
    region = poland_regions$region[which.min(geosphere::distHaversine(cbind(longitudeDecimal, latitudeDecimal), 
                                                                      cbind(poland_regions$lon, poland_regions$lat)))]
  ) %>%
  ungroup()


# Define the theme using bslib with custom colors and styles
my_theme <- bs_theme(
  base_font = font_link("Cabin", href = "https://fonts.googleapis.com/css2?family=Cabin:ital,wght@0,400..700;1,400..700&display=swap")
)


{
  
  
  
  create_species_analysis_module <- function(input, output, session, selected_species) {
    
    # Reactive value to store the filtered species data
    species_data <- reactive({
      
      req(selected_species())
      
      x <- biodata %>%
        filter(scientificName == selected_species() | vernacularName == selected_species()) %>% 
        filter(if (input$only_with_image) !(accessURI == "https://upload.wikimedia.org/wikipedia/commons/1/14/No_Image_Available.jpg") else TRUE) %>% 
        filter(eventDate >= input$date_range_input[1] & eventDate <= input$date_range_input[2]) %>%
        filter(region %in% input$region_input) %>%
        filter(sex %in% input$sex_input)
      
      return(x)
      
    })
    
    # Render UI for species image
    output$species_image <- renderUI({
      
      req(species_data())
      
      if (length(unique(species_data()$accessURI)) > 1) {
        filtered_uri <- species_data() %>%
          filter(accessURI != "https://upload.wikimedia.org/wikipedia/commons/1/14/No_Image_Available.jpg") %>%
          select(accessURI) %>%
          pull()
        selected_uri <- filtered_uri[1]
      } else {
        selected_uri <- unique(species_data()$accessURI)[1]
      }
      
      img(src = selected_uri, width = "100%")
      
    })
    
    # Render UI for species info
    output$species_info <- renderUI({
      
      req(species_data())
      
      region_count <- species_data() %>%
        count(scientificName, region) %>%
        arrange(desc(n)) %>%
        slice(1)
      top_region <- region_count$region[1]
      
      month_count <- species_data() %>%
        count(scientificName, region, month) %>%
        arrange(desc(n)) %>%
        slice(1)
      top_month <- month_count$month[1]
      
      info <- paste("The species ", selected_species(), 
                    " has been observed primarily in the ", top_region, 
                    " region, particularly during the month of ", top_month, ".")
      
      p(info)
    })
    
    # Render Leaflet map
    output$map <- renderLeaflet({
      species_map(species_data())
    })
    
    # Render Plotly plots
    output$plot1 <- renderPlotly({
      req(species_data())
      heatmap_plot(species_data())
    })
    
    output$plot2 <- renderPlotly({
      req(species_data())
      bar_chart(species_data())
    })
  }
  
  
  
  
  
  
  
  
}

# Module for Home Page
homeUI <- fluidPage(
  theme = my_theme,

  
    uiOutput("home"),
  
    uiOutput("search_results")
  
    
  
    
  
  )



# Define server
server <- function(input, output, session) {
  
  
  output$home <- renderUI({
    
    req(is.null(input$search_input))
    
    tags$div(
      
      tags$nav(class = "navbar navbar-expand-lg", style = "background-color: #2596be;",
               tags$div(
        class = "container-fluid",
        
        tags$a(
          class = "navbar-brand",
          href = "#",
          "Biodiversity App",
          style = "color:white;"
        ),
        
        tags$div(
          class = "ml-auto",
          
          tags$a(
            class = "nav-link",
            href = "https://mateocordobatoro.lat",
            "Check my portfolio!!",
            style = "color:white; text-align: right;"
          ),
        )
      )),  
      
      # search bar
    {tags$div(
      class = "container-fluid",
      tags$h1("Lets Explore Nature Across Poland!!!"),
      tags$p("Track, locate, and learn about Poland's wildlife"),
      style = "background-color: #2596be; color: white; text-align: center; height: 36vh; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      
      # Search bar
      tags$div(
        class = "input-group",
        style = "width: 50%; margin: 0 auto;",
        
        selectizeInput(
          inputId = "search_input",
          label = NULL,
          choices = unique(biodata$scientificName),
          multiple = TRUE,
          options = list(
            placeholder = "Search for a species...",
            maxOptions = 5,
            maxItems = 1
          ),
          width = '100%'
        )
      )
    )},
    
    # Top sighted species
    {tags$div(
      style = "height: 50vh",
      
      tags$div(
        class = "container-fluid",
        tags$h1("Top 3 Most Spotted Species"),
        tags$p("Catch a glimpse of the species most often spotted across Poland"),
        style = "text-align: center; margin-top: 20px;"
      ),
      
      # Top sighted species images and links
      {tags$div(
        class = "container",
        style = "width: 60%; margin: 0 auto; text-align: center;",
        
        tags$div(
          class = "row",
          
          # Image 1
          tags$div(
            class = "col-4",
            style = "padding: 10px;",
            tags$div(
              tags$img(src = "https://observation.org/photos/2395855.jpg", style = "width: 100%; height: auto;"),
              tags$p("Lanius collurio", style = "margin-top: 10px;")
            )
          ),
          
          # Image 2
          tags$div(
            class = "col-4",
            style = "padding: 10px;",
            tags$div(
              tags$img(src = "https://observation.org/photos/8014068.jpg", style = "width: 100%; height: auto;"),
              tags$p("Emberiza citrinella", style = "margin-top: 10px;")
            )
          ),
          
          # Image 3
          tags$div(
            class = "col-4",
            style = "padding: 10px;",
            tags$div(
              tags$img(src = "https://observation.org/photos/2413403.jpg", style = "width: 100%; height: auto;"),
              tags$p("Ciconia ciconia", style = "margin-top: 10px;")
            )
          )
        )
      )}
    )}
      
    )
    
  })
  
  
  output$search_results <- renderUI({
    
    req(input$search_input)
    
    tags$div(
      
      # Custom CSS para el diseño
      {tags$style(HTML("
    /* Mapa Leaflet ocupando toda la pantalla */
    
    
    #map {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: 1;
    }
    /* Panel flotante de la izquierda */
    #left_panel {
      position: absolute;
      top: 55%;
      left: 20px;
      transform: translateY(-50%);
      background-color: rgba(128, 128, 128, 0.8);
      paddin-top: 10px;
      padding: 10px;
      border-radius: 15px;
      z-index: 1000;
      width: 300px;
      color: white;
    }
    /* Panel flotante de la derecha que se ajusta al contenido */
    #overlay_container {
      position: absolute;
      top: 100px;
      right: 20px;
      background-color: rgba(255, 255, 255, 0.8);
      padding: 15px;
      border-radius: 15px;
      z-index: 1000;
      width: 400px;
      max-height: calc(100vh - 150px);
      overflow-y: auto;
      transition: max-height 0.3s ease;
    }
    #overlay_container.minimized {
      max-height: 40px;
      overflow: hidden;
      padding: 5px;
    }
    /* Imagen y gráficos deben ajustarse al tamaño */
    .resizable-content {
      margin-bottom: 10px;
      transition: max-height 0.3s ease;
      overflow: hidden;
    }
    img {
      max-width: 100%;
      height: auto;
      display: block;
      margin-left: auto;
      margin-right: auto;
    }
    /* Botones de minimización */
    .minimize-button {
      cursor: pointer;
      background-color: #eee;
      padding: 5px;
      text-align: center;
      border-radius: 5px;
      margin-bottom: 5px;
    }
  "))},
      
      # Panel lateral flotante con filtros
      {div(id = "left_panel",
           tags$h3("Search species"),
           selectInput("search_ui_input", "Scientific name", choices = unique(biodata$scientificName), multiple = FALSE, selected = input$search_input),
           dateRangeInput("date_range_input", "Date range", start = min(biodata$eventDate), end = max(biodata$eventDate)),
           selectInput("region_input", "Region", choices = c(unique(biodata$region)), multiple = TRUE, selected = unique(biodata$region)),
           checkboxGroupInput("sex_input","Sex", choices = unique(biodata$sex), selected = unique(biodata$sex), inline = TRUE),
           checkboxInput("only_with_image", "Just show data points with pictures available")
      )},
      
      # Contenedor flotante encima del mapa con gráficos e imagen
      {div(id = "overlay_container",
           div(class = "minimize-button", "Hide panel", onclick = "togglePanel()"),
           
           # Imagen con texto dinámico debajo
           div(class = "resizable-content", id = "image_content",
               div(id = "image_container",
                   uiOutput("species_image"),
                   uiOutput("species_info")
               )
           ),
           
           # Gráfico 1 (abierto por defecto)
           div(class = "resizable-content", id = "plot1_content",
               div(id = "plot1_container", plotlyOutput("plot1", height = "200px"))
           ),
           
           # Gráfico 2 (abierto por defecto)
           div(class = "resizable-content", id = "plot2_content",
               div(id = "plot2_container", plotlyOutput("plot2", height = "200px"))
           )
      )},
      
      # Mapa Leaflet que ocupa toda la pantalla
      {leafletOutput("map",
                     height = "100%",
                     width = "100%")},
      
      # Script para ocultar/mostrar el panel completo
      {tags$script(HTML("
    function togglePanel() {
      var panel = document.getElementById('overlay_container');
      if (panel.classList.contains('minimized')) {
        panel.classList.remove('minimized');
        document.querySelector(`[onclick=\"togglePanel()\"]`).innerHTML = 'Minimizar Panel';
      } else {
        panel.classList.add('minimized');
        document.querySelector(`[onclick=\"togglePanel()\"]`).innerHTML = 'Ampliar Panel';
      }
    }
  "))}
      

      
    )
    
    
  })
  
  
  
  # search results server 
  
  create_species_analysis_module(input, output, session, selected_species = reactive(input$search_ui_input))

  
}

# Run the app
shinyApp(ui = homeUI, server)
