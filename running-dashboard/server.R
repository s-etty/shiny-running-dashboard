library(shiny)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(geosphere)
library(ggplot2)

path <- file.path("data")
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

most_recent_date <- unique(date(points$time))
recorded_dates <- unique(date(tracks$time))
min_date <- min(recorded_dates)
n_days <- interval(min_date, most_recent_date)/days(1)
possible_dates <- min_date + days(0:n_days)
disabled_dates <- anti_join(x = as_tibble(possible_dates), y = as_tibble(recorded_dates))
disabled_dates <- pull(disabled_dates, value)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$mymap <- renderLeaflet({
        points <- tracks %>%
            filter(date(time) == input$date)
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolylines(lng = points$lon, lat = points$lat)
    })
    
    output$elevation <- renderPlotly({
        points <- tracks %>%
            filter(date(time) == input$date)
        
        plot <- plot_ly(
            x = points$time,
            y = points$elevation
        ) %>%
            add_lines(name = "Elevation") %>%
            add_lines(name = "Speed", y = points$speed, color = "red") %>%
            layout(paper_bgcolor = "#2B2E32", plot_bgcolor = "#2B2E32",
                   xaxis = list(
                       color = "white",
                       showgrid = FALSE
                   ),
                   yaxis = list(
                       color = "white",
                       title = "Elevation (m)",
                       showgrid = FALSE
                   ),
                   legend = list(
                       font = list(
                           color = "white"
                       )
                   ))
    })
})
