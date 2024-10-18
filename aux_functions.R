
library(leaflet)
library(leaftime)
library(htmltools)
library(readr)
library(dplyr)
library(jsonlite)

data <- read_csv("data/poland_data_sample.csv")


df <-  data %>%
  filter(vernacularName == "Red-backed Shrike") %>%
  select(latitudeDecimal, longitudeDecimal, eventDate)


timeline_map <- function(data,lat,long){
  
  date_aux <- data %>%
    distinct(eventDate) %>%
    arrange(eventDate) %>%
    mutate(start = seq.Date(from = min(data$eventDate), by = "day", length.out = n()),
           end = max(start))
  
  
  data <- data %>%
    left_join(date_aux, by = c("eventDate" = "eventDate"))
  
  data_json <- toJSON(date_aux, dataframe = "rows", auto_unbox = TRUE)
  
  
  
  
  
  js_query <- "function(date) {return new Date(date).toDateString();}"
  
  
  
  species_geo <- geojsonio::geojson_json(data,lat=lat,lon=long)
  
  
  g <- leaflet(species_geo) %>%
    addTiles() %>%
    setView(lng = 19.4, lat = 52.1, zoom = 6.4) %>%  # Centrar en Polonia
    addTimeline(
      sliderOpts = sliderOptions(
        #formatOutput = htmlwidgets::JS("function(date) {return new Date(date)}"),
        formatOutput =  htmlwidgets::JS(js_query),
        position = "bottomright",
        duration = 10000*10,
        showTicks = FALSE
      )
      ,
      timelineOpts = timelineOptions(
        styleOptions = markerOptions(
          radius = 10,
          color = "#ff7800",
          fillColor = "#ff7800",
          fillOpacity = 0.8
        )
      ))
  
  
  
  return(g)
  
}


timeline_map1 <- timeline_map(df,'latitudeDecimal', 'longitudeDecimal')


