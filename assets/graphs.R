

species_map <- function(df,long_set_view = 19.1451,lat_set_view = 52.2370){
  

    map <- df %>% 
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      # Add modern tile style (e.g., CartoDB Positron)
      addTiles() %>% 
      #addProviderTiles(providers$CartoDB.Positron) %>%
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
        popup = ~paste0(paste0("<img src=",accessURI," width='100' height='100'>"),
                        "<b>Species: </b>", scientificName, "<br>",
                        "<b>Sight date: </b>",eventDate, "<br>",
                        "<b>Latitude: </b>", round(latitudeDecimal,2), "<br>",
                        "<b>Longitude: </b>", round(longitudeDecimal,2)
                        ),
        popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE) # Popups stay open
        
      ) %>%
      # Optional: Add an attribution for the map provider
      addControl(html = "Map data &copy; <a href='https://mateocordobatoro.lat'> Mateo Cordoba</a> contributors", 
                 position = "bottomright", 
                 className = "leaflet-control-attribution")
    
    return(map)

  
}  
  
heatmap_plot <- function(data){
  
  data <- data %>%
    mutate(year = format(as.Date(eventDate), "%Y"),
           month)
  

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
      title = list(text = "<b>Sightings Heatmap</b>", font = list(size = 18)),  # Bold title
      xaxis = list(
        showticklabels = TRUE,  # Hide month labels
        showgrid = TRUE,        # Hide grid lines
        zeroline = TRUE,        # Hide axis line
        ticks = '*'               # Hide ticks
      ),
      yaxis = list(
        showticklabels = TRUE,  # Hide year labels
        showgrid = TRUE,        # Hide grid lines
        zeroline = TRUE,        # Hide axis line
        ticks = '*'               # Hide ticks
      ),
      font = list(family = "Arial", size = 10)  # Font settings for aesthetics
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
      title = list(text = "<b>Sightings by Year and Sex</b>", font = list(size = 18)),  # Bold title
      xaxis = list(title = "Year"),  # X-axis label
      yaxis = list(title = "Total Views"),  # Y-axis label renamed to "Total Views"
      barmode = "group",  # Grouped bars by sex
      font = list(family = "Arial", size = 14),  # Font settings for consistency
      showlegend = TRUE  # Show the legend by default
    )
  
  return(bar_chart)
}



