# library(shiny)
# library(dplyr)
# library(readr)
# library(shinyjs)
# library(shinybusy)
# library(ggplot2)
# library(plotly)
# library(leaflet)
# 
# 
# data <- read_csv("~/MateoDS/biodiversity_app/data/poland_data_sample.csv")
# 
# 
# data <- data %>% 
#   select(scientificName,vernacularName,longitudeDecimal, latitudeDecimal, eventDate, sex) %>% 
#   filter(scientificName %in% c("Lanius collurio",        
#                                "Grus grus ",             
#                                "Emberiza citrinella",    
#                                "Ciconia ciconia" ,       
#                                "Carpodacus erythrinus")) 
# 
# 
# 
# specie_name <- "Lanius collurio"
# 
#   df <-  data %>% filter(scientificName == specie_name)
#   long_set_view <- mean(df$longitudeDecimal, na.rm = TRUE)
#   lat_set_view  <- mean(df$latitudeDecimal, na.rm = TRUE)
#   
#   long_set_view <- 19.1451  # Longitude center of Poland
#   lat_set_view  <- 52.2370  # Latitude center of Poland

  
  
species_map <- function(df,long_set_view = 19.1451,lat_set_view = 52.2370){
  
  df %>% 
    leaflet() %>% 
    # Add modern tile style (e.g., CartoDB Positron)
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Set view to the center of Poland
    setView(lng = long_set_view, lat = lat_set_view, zoom = 6) %>%
    # Add circle markers with modern aesthetics
    addCircleMarkers(
      lng = ~longitudeDecimal,
      lat = ~latitudeDecimal,
      radius = 6, # Slightly larger circles
      color = "#007bff", # Modern blue color
      stroke = TRUE, # Add stroke/border
      weight = 1, # Border thickness
      fillColor = "#007bff", # Fill color matches the stroke
      fillOpacity = 0.7, # Some transparency for a modern feel
      popup = ~paste0("<b>Species: </b>", scientificName, "<br>",
                      "<b>Sight date: </b>",eventDate, "<br>",
                      "<b>Latitude: </b>", round(latitudeDecimal,2), "<br>",
                      "<b>Longitude: </b>", round(longitudeDecimal,2)),
      popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE) # Popups stay open
      
    ) %>%
    # Optional: Add an attribution for the map provider
    addControl(html = "Map data &copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors", 
               position = "bottomright", 
               className = "leaflet-control-attribution")
}  
  
heatmap_plot <- function(data){
  
  data <- data %>%
    mutate(year = format(as.Date(eventDate), "%Y"),
           month = format(as.Date(eventDate), "%m"))
  

  # Aggregating sightings by year and month
  sightings_matrix <- data %>%
    group_by(year, month) %>%
    summarise(sightings = n()) %>%
    ungroup() 
  

  
  # Create a matrix plot (heatmap)
  heatmap_plot <- plot_ly(
    data = sightings_matrix,
    x = ~month, 
    y = ~year, 
    z = ~sightings_matrix$sightings, 
    type = "heatmap",
    colors = c("#f7fbff", "#08519c"), # Light to dark blue gradient for modern aesthetics
    hoverinfo = 'text',  # Display custom text on hover
    text = ~paste("Year: ", year,
                  "<br>Month: ", month,
                  "<br>Total Sights: ", sightings_matrix$sightings),
    showscale = FALSE # Completely hide the colorbar (this is the critical part)
  ) %>%
    layout(
      title = list(text = "<b>Species Sightings Heatmap</b>", font = list(size = 18)),  # Bold title
      xaxis = list(
        showticklabels = FALSE,  # Hide month labels
        showgrid = FALSE,        # Hide grid lines
        zeroline = FALSE,        # Hide axis line
        ticks = ''               # Hide ticks
      ),
      yaxis = list(
        showticklabels = FALSE,  # Hide year labels
        showgrid = FALSE,        # Hide grid lines
        zeroline = FALSE,        # Hide axis line
        ticks = ''               # Hide ticks
      ),
      font = list(family = "Arial", size = 14)  # Font settings for aesthetics
    )
  
  heatmap_plot
}

bar_chart <- function(data){
  
  data <- data %>%
    mutate(year = format(as.Date(eventDate), "%Y"))
  
  # Aggregate sightings by year and sex
  sightings_by_year_sex <- data %>%
    group_by(year, sex) %>%
    summarise(sightings = n()) %>%
    ungroup()
  
  # Create a bar chart that toggles between total sightings and colored by sex
  bar_chart <- plot_ly(
    data = sightings_by_year_sex,
    x = ~year, 
    y = ~sightings, 
    color = ~sex,  # Color by sex
    type = "bar",
    text = ~paste("Total Views: ", sightings, "<br>Year: ", year, "<br>Sex: ", sex),  # Custom hover text
    hoverinfo = "text",  # Show only the custom text in the hover popup
    colors = c("#007bff", "#f44336"),  # Modern colors: blue for male, red for female
    marker = list(line = list(width = 1, color = '#FFFFFF')) # White border for clean look
  ) %>%
    layout(
      title = list(text = "<b>Species Sightings by Year</b>", font = list(size = 18)),  # Bold title
      xaxis = list(title = "Year"),  # X-axis label
      yaxis = list(title = "Total Views"),  # Y-axis label renamed to "Total Views"
      barmode = "group",  # Grouped bars by sex
      font = list(family = "Arial", size = 14),  # Font settings for consistency
      showlegend = TRUE  # Show the legend by default
    )
  
  return(bar_chart)
}



