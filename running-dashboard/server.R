library(shiny)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(geosphere)
library(ggplot2)

imperial_metric <- function(measurement, units) {
    #convert metric to imperial
    #the units argument is what unit the measurement was recorded in, e.g., recorded in 
    #m/s need to go to mph
    imperial_measurement <- case_when(
        units == "m/s" ~ measurement * 2.3694,
        #convert to mph
        units == "m" ~ measurement * 3.28084,
        #convert to ft, useful for elevation
        units == "km" ~ measurement * 0.621371,
        #convert to miles
        units == "meters" ~ measurement * 0.000621371
        #convert from meters to miles
    )
}

#read the csv files
path <- file.path("data")
csv_files <- fs::dir_ls(path, regexp = "\\.csv$")
#create a fs_path object of all the csv files in the above directory (data/...)
tracks <- csv_files %>%
    map_dfr(read_csv) %>%
    #map the read_csv function to the fs_path object "csv_files", i.e., read all the csv's
    rename(track_timestamp = time) %>%
    #rename the time column to track_timestamp to avoid function name conflict
    mutate(track_timestamp = with_tz(track_timestamp, tzone = Sys.timezone())) %>%
    #add the system timezone to the timestamp, may run into issues here when deploying
    #to a server. Will it use the server's timezone?
    mutate(elevation = imperial_metric(elevation, "m")) %>%
    #change the elevation to ft
    mutate(speed = imperial_metric(speed, "m/s"))
    #change the speed to mph

#group the tracks by date, then generate an id for each track
track_id_df <- tracks %>%
    mutate(track_date = date(track_timestamp)) %>%
    #extract the date from the timestamps
    group_by(track_date) %>%
    #group them by track dates, will not work for days with multiple workouts
    group_indices(track_date)
    #assign an ID to each group

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
    mutate(runtime_min = difftime(first(track_timestamp), track_timestamp, units = "mins")) %>%
    #calculate elapsed time
    mutate(runtime_sec = difftime(first(track_timestamp), track_timestamp, units = "secs")) %>%
    #calculate elapsed time in seconds, easier for some calculations
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

shinyServer(function(input, output) {
    #create a date selector to filter which tracks to show
    output$date_input <- renderUI({
        dateInput("date", label = h5("Select a Date"),
                  value = most_recent_date,
                  min = min_date, max = most_recent_date,
                  datesdisabled = disabled_dates)
    })
    
    output$mymap <- renderLeaflet({
        #create a map to plot the tracks onto
        if(!is.null(input$date)){
        #because JS is asynchronous, this will load before some of the above code finishes
        #flasing a short error message if you don't include the if statement. kind of hackey
            points <- tracks %>%
                filter(date(track_timestamp) == input$date)
            #filter using the date selected from the dateInput above
            leaflet() %>%
                addProviderTiles(providers$Stamen.TonerLite) %>%
                #tells which map tiles to use
                addPolylines(lng = points$lon, lat = points$lat,
                             col = "#CD5C5C")
            #plot each lat and lon as a point in the polygon
        }
    })
    
    output$elevation_speed <- renderPlotly({
        #create the elevation and speed plots
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
                           color = "#f0ebd8",
                           showgrid = FALSE,
                           title = "Elapsed Runtime (min)"
                       ),
                       legend = list(
                           font = list(
                               color = "#f0ebd8"
                           )
                       ),
                       yaxis = list(
                           color = "#f0ebd8",
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
                           color = "#f0ebd8",
                           showgrid = FALSE,
                           title = "Elapsed Runtime (min)"
                       ),
                       legend = list(
                           font = list(
                               color = "#f0ebd8"
                           )
                       ),
                       yaxis = list(
                           color = "#f0ebd8",
                           title = "Speed (mph)",
                           showgrid = FALSE
                       )
                )
            
            elevation_speed <- subplot(elevation_plot, speed_plot,
                                       nrows = 2, shareX = TRUE,
                                       titleY = TRUE)
        }
    })
    
    output$current_summary_stats <- renderUI({
        if(!is.null(input$date)){
            summary_stats <- tracks %>%
                filter(date(track_timestamp) == input$date) %>%
                summarize(total_distance = sum(distance, na.rm = TRUE),
                          avg_speed = total_distance / (max(as.numeric(runtime_min)) / 60),
                          max_speed = max(speed, na.rm = TRUE),
                          pace = (max(as.numeric(runtime_min))) / total_distance,
                          pace = hms(chron::times(pace)/ (24 * 60)),
                          max_elevation = max(elevation, na.rm = TRUE),
                          total_runtime = as.period(round(max(runtime_sec, na.rm = TRUE)))
                )
            
            vertical_gain <- tracks %>%
                filter(date(track_timestamp) == input$date) %>%
                mutate(elevation_change = lead(elevation) - elevation,
                       track_timestamp_15s = round_date(track_timestamp, unit = "15s")) %>%
                group_by(track_timestamp_15s) %>%
                tidyr::drop_na(elevation_change) %>%
                summarize(vertical_gain_15s = mean(elevation_change)) %>%
                summarize(vertical_gain = sum(vertical_gain_15s[vertical_gain_15s > 0]))
            
            div(class = "summary-stats",
                h3("Session Stats"),
                div(class = "col-sm-6",
                    p(class = "stats", sprintf("%.2f", summary_stats$total_distance),
                      icon("ruler", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Total Miles"),
                    br(),
                    p(class = "stats", sprintf("%.1f", summary_stats$avg_speed),
                      icon("tachometer-alt", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Avg MPH"),
                    br(),
                    p(class = "stats", sprintf("%.0f", summary_stats$max_elevation),
                      icon("mountain", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Max Elevation ft")
                ),
                div(class = "col-sm-6",
                    p(class = "stats",
                      sprintf("%02d:%02d", minute(summary_stats$total_runtime),
                                               second(summary_stats$total_runtime)),
                      icon("stopwatch", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Runtime"),
                    br(),
                    p(class = "stats",
                      sprintf("%02d:%02d", minute(summary_stats$pace),
                              second(summary_stats$pace)),
                      icon("bolt", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Pace Min/Mi"),
                    br(),
                    p(class = "stats", sprintf("%.0f", vertical_gain$vertical_gain),
                      icon("level-up-alt", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Vertical Gain ft")
                )
            )
            #the vertical gain is not working
        }
    })
})