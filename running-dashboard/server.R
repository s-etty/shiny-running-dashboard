library(shiny)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(geosphere)
library(ggplot2)

imperial_metric <- function(measurement, units) {
    imperial_measurement <- case_when(
        units == "m/s" ~ measurement * 2.3694,
        units == "m" ~ measurement * 3.28084,
        units == "km" ~ measurement * 0.621371,
        units == "meters" ~ measurement * 0.000621371 #convert from meters to miles
    )
}

#read the csv files
path <- file.path("data")
csv_files <- fs::dir_ls(path, regexp = "\\.csv$")
tracks <- csv_files %>%
    map_dfr(read_csv) %>%
    rename(track_timestamp = time) %>%
    mutate(track_timestamp = with_tz(track_timestamp, tzone = Sys.timezone())) %>%
    mutate(elevation = imperial_metric(elevation, "m")) %>%
    mutate(speed = imperial_metric(speed, "m/s"))

#group the tracks by date, then generate an id for each track
track_id_df <- tracks %>%
    mutate(track_date = date(track_timestamp)) %>%
    group_by(track_date) %>%
    group_indices(track_date)

#prepare the tracks df for display
tracks <- tracks %>%
    mutate(track_id = cbind(track_id_df)) %>%
    #add a track_id column, may go away when connect to a database
    arrange(desc(track_timestamp)) %>%
    #sort descending date, i.e., newest at the top
    distinct(track_timestamp, .keep_all = TRUE) %>%
    #drop any duplicate rows
    group_by(track_id) %>%
    #group the tracks together
    mutate(runtime = difftime(first(track_timestamp), track_timestamp, units = "mins")) %>%
    #calculate elapsed time
    mutate(nextLat = lead(lat), nextLon = lead(lon)) %>%
    #create columns for the next lat and lon to calculate distance between points
    rowwise() %>%
    mutate(distance = distVincentyEllipsoid(c(lon, lat), c(nextLon, nextLat))) %>%
    #calculate distances between points
    ungroup() %>%
    mutate(distance = imperial_metric(distance, "meters"))

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
    
    output$elevation_speed <- renderPlotly({
        if(!is.null(input$date)){
            points <- tracks %>%
                filter(date(track_timestamp) == input$date) %>%
                mutate(track_timestamp_30s = round_date(track_timestamp, unit = "30s")) %>%
                group_by(track_timestamp_30s) %>%
                mutate(elevation_30s = mean(elevation),
                          speed_30s = mean(speed)) %>%
                ungroup()
            
            elevation_plot <- plot_ly(points,
                x = ~track_timestamp_30s,
                y = ~elevation_30s
            ) %>%
                add_lines(name = "Elevation") %>%
                layout(paper_bgcolor = "#242729", plot_bgcolor = "#242729",
                       xaxis = list(
                           color = "white",
                           showgrid = FALSE,
                           title = "Elapsed Runtime (min)"
                       ),
                       legend = list(
                           font = list(
                               color = "white"
                           )
                       ),
                    yaxis = list(
                        color = "white",
                        title = "Elevation (ft)",
                        showgrid = FALSE
                    )
                )
            
            speed_plot <- plot_ly(points,
                x = ~track_timestamp_30s,
                y = ~speed_30s
            ) %>%
                add_lines(name = "Speed") %>%
                layout(paper_bgcolor = "#242729", plot_bgcolor = "#242729",
                       xaxis = list(
                           color = "white",
                           showgrid = FALSE,
                           title = "Elapsed Runtime (min)"
                       ),
                       legend = list(
                           font = list(
                               color = "white"
                           )
                       ),
                       yaxis = list(
                           color = "white",
                           title = "Speed (mph)",
                           showgrid = FALSE
                       )
                )
            
            elevation_speed <- subplot(elevation_plot, speed_plot,
                                       nrows = 2, shareX = TRUE,
                                       titleY = TRUE)
        }
    })
})
