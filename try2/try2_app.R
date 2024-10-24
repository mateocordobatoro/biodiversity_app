library(shiny)
library(dplyr)
library(readr)
library(shinyjs)
library(shinybusy)
library(leaflet)
library(plotly)


biodata <- read_csv("~/MateoDS/biodiversity_app/data/poland_data_sample.csv")

source("~/MateoDS/biodiversity_app/try2/graphs/graphs.R")

biodata <- data %>%
  filter(scientificName %in% c("Lanius collurio",
                             "Grus grus ",
                             "Emberiza citrinella",
                             "Ciconia ciconia" ,
                             "Carpodacus erythrinus"))



ui_search_bar <- function(id, choices, place_holder){


  selectizeInput(
    inputId = id,
    label = "",
    multiple = FALSE,
    choices = c("Search Bar" = "", choices),
    options = list(
      create = FALSE,
      placeholder = place_holder,
      maxItems = '1',
      onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
      onType = I("function (str) {if (str === \"\") {this.close();}}")
    ),
    width = '40%'
  )

}

search_ui <- tagList(

  htmlTemplate('index.html',
               search_ui_search_bar = ui_search_bar('search_ui_search_bar',
                                                    unique(data$scientificName),
                                                    'scientific or vernacular name ...'),
               input1 = selectInput('species', 'Species', choices = c("Lanius collurio",
                                                             "Grus grus ")),
               input2 = selectInput('year', 'Year', choices = c(2010, 2011, 2012, 2013, 2014,
                                                       2015, 2016, 2017, 2018, 2019)),
               input3 = selectInput('month', 'Month', choices = c(1, 2, 3, 4, 5, 6, 7,
                                                                  8, 9, 10, 11, 12)),
               input4 = selectInput('day', 'Day', choices = c(1, 2, 3, 4, 5, 6, 7)),
               filter_button = actionButton('filter_button', 'Filter', icon = icon('filter')),
               map = leafletOutput('map', height = '100%', width = '100%'),
               plot1 = plotlyOutput('plot1'),
               plot2 = plotlyOutput('plot2')
               )

)

# Home ui -----

home_ui <- tagList(
  # Include external CSS file
  includeCSS("www/style1.css"),
  
  # Page title (equivalent to <title> in HTML)
  tags$head(tags$title("Biodiversity Searcher")),
  
  # Navbar and login buttons
  tags$header(
    tags$div(class = "navbar",
             tags$a(href = "#", class = "logo", "Biodiversity App"),
             tags$nav(
               tags$ul(
                 tags$li(tags$a(href = "#", "Home")),
                 tags$li(tags$a(href = "#", "Species Reports")),
                 tags$li(tags$a(href = "#", "Blog"))
               )
             ),
             tags$div(class = "login-buttons",
                      tags$button(class = "login-btn", "Login"),
                      tags$button(class = "post-btn", "Post a Sighting")
             )
    )
  ),
  
  # Hero section with search input and search button
  tags$section(class = "hero",
               tags$h1("Discover Biodiversity Around You!"),
               tags$p("Thousands of species sightings across Poland."),
               tags$div(class = "search-bar",
                        selectizeInput(
                          inputId = "search_input", 
                          label = "",
                          multiple = FALSE,
                          choices = c("Search Bar" = "", unique(c(biodata$scientificName, biodata$vernacularName))),
                          options = list(
                            create = FALSE,
                            placeholder = "scientific or vernacular name ...",
                            maxItems = '1',
                            onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                            onType = I("function (str) {if (str === \"\") {this.close();}}")
                          ),
                          width = '40%'
                        )

               )
  ),
  
  # Popular species section
  tags$section(class = "popular-cities",
               tags$h2("Popular Species"),
               tags$p("Find your next wildlife encounter across Poland!"),
               tags$div(class = "city-grid",
                        tags$div(class = "city-item",
                                 tags$img(src = "img/species1.jpg", alt = "Species 1"),
                                 tags$h3("White Stork")
                        ),
                        tags$div(class = "city-item",
                                 tags$img(src = "img/species2.jpg", alt = "Species 2"),
                                 tags$h3("European Bison")
                        ),
                        tags$div(class = "city-item",
                                 tags$img(src = "img/species3.jpg", alt = "Species 3"),
                                 tags$h3("Gray Wolf")
                        )
               )
  ),
  
  # 
  # leafletOutput("map"),
  # plotlyOutput("plot1"),
  # plotlyOutput("plot2"),
  
  # Include external JS file
  tags$script(src = "www/script.js")
)

ui <- tagList(
  uiOutput("dynamic_ui")
)

# Server (if needed)
server <- function(input, output, session) {
  
  # Reactive value to store the selected species
  selected_species <- reactiveVal(NULL)

  # Observe the species selection
  observeEvent(input$search_input, {
    selected_species(input$search_input)
  })


  # Render the UI dynamically based on whether a species has been selected
  output$dynamic_ui <- renderUI({
    
    if (is.null(selected_species()) || selected_species() == "") {
      home_ui  # Show home page if no species is selected
    } else {
      #home_ui
      search_ui  # Show search results page if a species is selected


    }

  })


  
  ##### search results server ---------
  
  
  
  species_data <- reactive({
    
    
    x <- biodata %>% filter(scientificName == selected_species() | vernacularName == selected_species())
    
    return(x)
    
  })
  
  output$map <- renderLeaflet({
    
    req(selected_species())
    
    species_map(species_data())
    
  })
  
  
  output$plot1 <- renderPlotly({
    
    req(selected_species())
    
    heatmap_plot(species_data())
    
  })
  
  output$plot2 <- renderPlotly({
    
    req(selected_species())
    
    bar_chart(species_data())
    
  })
  
}
# Run the app
shinyApp(ui = ui, server = server)
