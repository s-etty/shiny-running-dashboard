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
    map_dfr(read_csv) %>%
    rename(track_timestamp = time)
#read the csv files

track_id_df <- tracks %>%
    mutate(track_date = date(track_timestamp)) %>%
    group_by(track_date) %>%
    group_indices(track_date)
#group the tracks by date, then generate an id for each track

tracks <- tracks %>%
    mutate(track_id = cbind(track_id_df)) %>%
    arrange(desc(track_timestamp)) %>%
    group_by(track_id) %>%
    mutate(nextLat = lead(lat), nextLon = lead(lon)) %>%
    rowwise() %>%
    mutate(distance = distVincentyEllipsoid(c(lon, lat), c(nextLon, nextLat)))


#bind the track ids into a new column then sort by newest first

#points <- tracks %>%
#    filter(track_id == tracks$track_id[1])
#select the most recent track as the first points to be drawn

#get the important tracks date info
most_recent_date <- date(tracks$track_timestamp[1])
#get the most recent recording
recorded_dates <- unique(date(tracks$track_timestamp))
#get the unique recorded dates
min_date <- min(recorded_dates)
#get the min recorded date
n_days <- interval(min_date, most_recent_date)/days(1)
#count the number of days between oldest and most recent recording
possible_dates <- min_date + days(0:n_days)
#dates between the min and most recent
disabled_dates <- anti_join(x = as_tibble(possible_dates), y = as_tibble(recorded_dates)) %>%
    pull(value)
#anti-join the possible dates with dates actually recorded


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$date_input <- renderUI({
            dateInput("date", label = h5("Select a Date"),
                      #value = "2020-08-27",
                      value = most_recent_date,
                      min = min_date, max = most_recent_date,
                      datesdisabled = disabled_dates)
    })
    
    output$mymap <- renderLeaflet({
        if(!is.null(input$date)){
            points <- tracks %>%
                filter(date(track_timestamp) == input$date)
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolylines(lng = points$lon, lat = points$lat)
        }
    })
    
    output$elevation <- renderPlotly({
        if(!is.null(input$date)){
            points <- tracks %>%
                filter(date(track_timestamp) == input$date)
            
            plot <- plot_ly(
                x = points$track_timestamp,
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
        }
    })
})
