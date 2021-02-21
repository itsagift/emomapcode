library(tidyverse)
library(ggmap)
library(mapview)
library(sf)
library(leaflet)
library(dplyr)
library(readxl)
library(readr)

run_results_12 <- read_excel("~/Downloads/run_results-12.xlsx")
sources <- distinct(run_results_12, Location)
sources_df <- as.data.frame(sources)
locations_df <- mutate_geocode(sources_df, Location)
locations <- as_tibble(locations_df)
per_source <- run_results_12 %>%
group_by(Location) %>%
summarise(count = n()) %>%
arrange(desc(count))
locations <- left_join(run_results_12, locations, by = c("Location" = "Location"))

geo_merge <- locations %>%
  dplyr::group_by(Location) %>%
  dplyr::summarise(BandName = paste(BandName, collapse = "<br>"))

geo_merge <- left_join(geo_merge, locations_df, by = c("Location" = "Location"))

geo_merge <- left_join(geo_merge, per_source, by = c("Location" = "Location"))
mytext <- paste(
  "<b>", geo_merge$Location, "</b><br/>",
  geo_merge$count," bands <br/>",
  "<p style= font-size:8px;>", geo_merge$BandName, "</p>") %>%
lapply(htmltools::HTML)
pal <- colorNumeric(c("#FFEDA1", "#FEB24C", "#FD8D3D", "#E3211C"), domain = geo_merged$count)

m <- leaflet(geo_merge) %>%
  addTiles()  %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  setView(-50, 27, zoom = 3.2) %>%
  addCircleMarkers(data=geo_merge, lng=~lon , lat=~lat, fillColor= ~pal(count), radius = ((sqrt(geo_merge$count/3.1415)) * 4), fillOpacity = 0.9, stroke = FALSE, label = mytext, labelOptions = labelOptions( style = list( "font-weight" = "normal", padding = "3px 8px", "width" = "150px", "max-height" = "400px", "overflow" = "scroll"), textsize = "11px", direction = "auto"))%>%
  addLegend( pal= pal, values=~geo_merge$count, opacity=0.9, title = "Bands", position = "bottomright" )
m














