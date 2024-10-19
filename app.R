library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


set.seed(123)  # Para reproducibilidad
latitudes <- runif(10, min = 49.0, max = 54.0)  # Rango aproximado de latitudes en Polonia
longitudes <- runif(10, min = 14.0, max = 24.0)  # Rango aproximado de longitudes en Polonia


# Define UI

ui <- tagList(
  
  useShinyjs(),
  htmlTemplate("index.html",
               plotOutput = plotOutput("myPlot"),
               plotOutput2 = plotOutput("anotherPlot"),
               plotOutput3 = plotOutput("plot3"),
               map = leafletOutput("map")
  
  
))


# Define server logic
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 19.1451, lat = 52.1625, zoom = 6) %>%  # Centro de Polonia
      addTiles() %>%  # Mapa base
      # Agregar marcadores aleatorios
      addMarkers(lng = longitudes, lat = latitudes, 
                 popup = paste("Marker", 1:10))
  })
  
  
  output$myPlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point() +
      labs(title = "Gráfica de Shiny en Template HTML")
  })
  
  output$anotherPlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point() +
      labs(title = "Gráfica de Shiny en Template HTMLlllllllll")
  })
  
  output$plot3 <- renderPlot({
    ggplot(mtcars, aes(x = wt)) +
      geom_histogram() +
      labs(title = "Gráfica de Shiny en Template HTMLlllllllll")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
