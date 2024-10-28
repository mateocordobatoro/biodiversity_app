# Biodiversity App for Species in Poland
This Shiny application visualizes biodiversity data, specifically focusing on species sightings across various regions in Poland. Users can track, locate, and analyze how often and when certain species are observed.

## App Features
- **Interactive Map:** Explore species sightings across different regions with an interactive map.
- **Filter Options:** Filter sightings by date range, region, and sex, or show only data points with images.
- **Species Analysis:** Get insights into the most observed region and month for each selected species.
- **Graphs and Visualizations:** Dynamic visualizations of species sightings, including a heatmap and bar charts.
  
## Data
The data used in this application is sourced from the Global Biodiversity Information Facility (GBIF), a global platform providing open access to biodiversity data. The dataset includes observations of various species around the world; however, for this exercise, we focus only on species occurrences in Poland.

For more information about the original dataset and GBIF's mission, visit the [GBIF website](https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165).







## Packages
The app utilizes several R packages to enable various functionalities:

- shiny: Provides the framework to create the web application.
- leaflet: Used for interactive maps to display species sightings across regions.
- plotly: Adds dynamic and interactive plotting capabilities, used here for graphs displaying species data.
- bslib: Offers custom styling and theming for a polished and user-friendly interface.
- dplyr: Provides data manipulation tools for filtering and transforming the dataset.
- geosphere: Calculates geographic distances to determine the closest region for each species sighting.
- lubridate: Handles date parsing and manipulation to filter data based on the date of sightings.

## How does it works?

The app’s main goal is to allow users to explore when and where specific species have been sighted in Poland. It can be especially useful for ecologists, researchers, or anyone interested in Polish biodiversity.

- **Search for a Species:** Use the search bar on the homepage to select a species of interest.
- **Filter Options:** Customize your view by filtering the sightings based on available options, such as date range and regions.
- **View Results:** The map displays locations of sightings, and the analysis panel shows related visualizations and images.
Screenshots

## Future Enhancements
- Include additional filters (e.g., habitat type).
- Expand data to include species sightings beyond Poland.
- Add temporal analysis to observe trends over time.
