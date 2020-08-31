library(shiny)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(geosphere)

path <- file.path("running-dashboard/data")
csv_files <- fs::dir_ls(path, regexp = "\\.csv$")
tracks <- csv_files %>%
    map_dfr(read_csv)
#read the csv files

track_id <- tracks %>%
    mutate(track_date = date(time)) %>%
    group_by(track_date) %>%
    group_indices(track_date)
#group the tracks by date, then generate an id for each track

tracks <- tracks %>%
    mutate(track_id = cbind(track_id)) %>%
    arrange(desc(time)) %>%
    group_by(track_id) %>%
    mutate(nextLat = lead(lat), nextLon = lead(lon)) %>%
    rowwise() %>%
    mutate(distance = distVincentyEllipsoid(c(lon, lat), c(nextLon, nextLat)))
#bind the track ids into a new column then sort by newest first

points <- tracks %>%
    filter(track_id == tracks$track_id[1])
#select the most recent track as the first points to be drawn

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$date <- renderPrint({input$date})
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolylines(lng = points$lon, lat = points$lat)
    })
})
