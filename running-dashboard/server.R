###########################################
#
# The code included in this file is the server side of this app.
# The app is used to show running statistics from GPS tracks.
# The app uses plotly and leaflet to render some of its visuals.
# Data transformations and manipulations happen with tidyverse packages (dplyr, readr, purrr,
# lubridate).
#
###########################################

library(shiny)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(geosphere)
library(ggplot2)
library(lutz)

# convert metric to imperial
# the units argument is the unit the measurement was recorded in, e.g., recorded in
# m/s need to go to mph
imperial_metric <- function(measurement, units) {
    imperial_measurement <- case_when(
        #convert to mph
        units == "m/s" ~ measurement * 2.3694,
        # convert to ft, useful for elevation
        units == "m" ~ measurement * 3.28084,
        # convert to miles
        units == "km" ~ measurement * 0.621371,
        # convert from meters to miles
        units == "meters" ~ measurement * 0.000621371
    )
}

# read the data file path and create a vector of the csv filenames
path <- file.path("data")
csv_files <- fs::dir_ls(path, regexp = "\\.csv$")

# create a fs_path object of all the csv files in the above directory (data/...)
tracks <- csv_files %>%
    # map the read_csv function to the fs_path object "csv_files", i.e., read all the csv's
    map_dfr(read_csv) %>%
    # rename the time column to track_timestamp to avoid function name conflict
    rename(track_timestamp = time) %>%
    # use the first tracks lat and lon to define the timezone.
    # there could be an opportunity to set the timezone for each track, but that will come later
    # also the reason for this is because if no timezone is specified it will use the server's
    # timezone when deployed to a shiny server and can cause errors
    mutate(track_timestamp = with_tz(track_timestamp, 
                                     tzone = tz_lookup_coords(lat[1], lon[1]))) %>%
    # change the elevation to ft
    mutate(elevation = imperial_metric(elevation, "m")) %>%
    # change the speed to mph
    mutate(speed = imperial_metric(speed, "m/s"))

# group the tracks by date, then generate an id for each track
track_id_df <- tracks %>%
    # extract the date from the timestamps
    mutate(track_date = date(track_timestamp)) %>%
    # group them by track dates, will not work for days with multiple workouts
    group_by(track_date) %>%
    # assign an ID to each group
    group_indices(track_date)

# prepare the tracks df for display
tracks <- tracks %>%
    # add a track_id column, may go away when connect to a database
    mutate(track_id = cbind(track_id_df)) %>%
    # round the timestamp to the nearest second. datetime is floating point so need to round so 
    # grouping works otherwise precision messes with it
    mutate(track_timestamp = round_date(track_timestamp, unit = "second")) %>%
    # take the mean of all numeric columns when there are two rows with the same timestamp
    # usually there are two points at the same time when one comes from gps and the other network
    group_by(track_timestamp) %>%
    summarize(across(where(is.numeric), mean)) %>%
    ungroup() %>%
    # sort descending date, i.e., newest at the top
    arrange(desc(track_timestamp)) %>%
    # group the tracks together
    group_by(track_id) %>%
    # calculate elapsed time
    mutate(runtime_min = difftime(first(track_timestamp), track_timestamp, units = "mins")) %>%
    # calculate elapsed time in seconds, easier for some calculations
    mutate(runtime_sec = difftime(first(track_timestamp), track_timestamp, units = "secs")) %>%
    # create columns for the next lat and lon to calculate distance between points
    mutate(prevLat = lag(lat), prevLon = lag(lon)) %>%
    # calculate distances between points
    rowwise() %>%
    mutate(distance = distVincentyEllipsoid(c(prevLon, prevLat), c(lon, lat))) %>%
    ungroup() %>%
    # convert to imperial. Distance will be in miles
    mutate(distance = imperial_metric(distance, "meters")) %>%
    # create a calculated speed because the speed from the gps is inaccurate
    mutate(time_btwn_pnts = as.numeric(difftime(lag(track_timestamp),
                                                track_timestamp,
                                                units = "hours")), 
           calc_speed = distance / time_btwn_pnts)

# get the most recent recording
most_recent_date <- date(tracks$track_timestamp[1])
# get the unique recorded dates
recorded_dates <- unique(date(tracks$track_timestamp))
# get the min recorded date
min_date <- min(recorded_dates)
# count the number of days between oldest and most recent recording
n_days <- interval(min_date, most_recent_date)/days(1)
#dates between the min and most recent
possible_dates <- min_date + days(0:n_days)
# anti-join the possible dates with dates actually recorded. This will be used to gray out
# selections on the calendar picker.
disabled_dates <- anti_join(x = as_tibble(possible_dates), y = as_tibble(recorded_dates)) %>%
    pull(value)


# call the shinyServer constructor to build the server side of the app
shinyServer(function(input, output) {
    
    # create a date selector to filter which tracks to show
    output$date_input <- renderUI({
        dateInput("date", label = h5("Select a Date"),
                  value = most_recent_date,
                  min = min_date, max = most_recent_date,
                  datesdisabled = disabled_dates)
    })
    
    # create a map to plot the tracks onto
    output$mymap <- renderLeaflet({
        # because JS is asynchronous, this will load before some of the above code finishes
        # flasing a short error message if you don't include the if statement. kind of hackey
        if(!is.null(input$date)){
            # filter using the date selected from the dateInput above
            points <- tracks %>%
                filter(date(track_timestamp) == input$date)
            
            leaflet() %>%
                # tells which map tiles to use
                addProviderTiles(providers$Stamen.TonerLite) %>%
                # plot each lat and lon as a point in the polyline shape
                addPolylines(lng = points$lon, lat = points$lat,
                             col = "#CD5C5C")
        }
    })
    
    # create the elevation and speed plots
    output$elevation_speed <- renderPlotly({
        # because JS is asynchronous, this will load before some of the above code finishes
        # flasing a short error message if you don't include the if statement. kind of hackey
        if(!is.null(input$date)){
            points <- tracks %>%
                filter(date(track_timestamp) == input$date) %>%
                # round the timestamp to the nearest 30s
                mutate(track_timestamp_30s = round_date(track_timestamp, unit = "30s")) %>%
                # group by 30s intervals
                group_by(track_timestamp_30s) %>%
                # get the mean elevation and speed for 30s intervals. This will
                # hopefully smooth out the plots while still maintaining some of
                # the resolution
                mutate(elevation_30s = mean(elevation),
                       speed_30s = mean(calc_speed)) %>%
                ungroup()
            
            # create the elevation plot
            # x axis is the elapsed time for each run
            # y axis is the elevation at that time
            elevation_plot <- plot_ly(points,
                                      x = ~runtime_min,
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
            
            # create the speed plot
            # x axis is the elapsed time for each run
            # y axis is the speed at that time
            speed_plot <- plot_ly(points,
                                  x = ~runtime_min,
                                  y = ~speed_30s
            ) %>%
                add_lines(name = "Speed") %>%
                layout(paper_bgcolor = "#242729", plot_bgcolor = "#242729",
                       xaxis = list(
                           color = "#f0ebd8",
                           showgrid = FALSE,
                           title = "Elapsed Runtime (min)",
                           rangemode='tozero'
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
            
            # put both the elevation and speed plot on the same subplot object
            # and have them share an x axis
            elevation_speed <- subplot(elevation_plot, speed_plot,
                                       nrows = 2, shareX = TRUE,
                                       titleY = TRUE)
        }
    })
    
    # create HTML renderings of session stats
    output$current_summary_stats <- renderUI({
        # because JS is asynchronous, this will load before some of the above code finishes
        # flasing a short error message if you don't include the if statement. kind of hackey
        if(!is.null(input$date)){
            # calculate the session summary stats
            summary_stats <- tracks %>%
                # filter tracks to the selected date
                filter(date(track_timestamp) == input$date) %>%
                # calculate summary stats
                summarize(total_distance = sum(distance, na.rm = TRUE),
                          avg_speed = total_distance / (max(as.numeric(runtime_min)) / 60),
                          max_speed = max(speed, na.rm = TRUE),
                          pace = (max(as.numeric(runtime_min))) / total_distance,
                          pace = hms(chron::times(pace)/ (24 * 60)),
                          max_elevation = max(elevation, na.rm = TRUE),
                          total_runtime = as.period(round(max(runtime_sec, na.rm = TRUE)))
                )
            
            ############ the vertical gain is not working
            # calculate the total vertical gain for a session
            # this includes all up and down movement, not just the max and min elevation difference
            vertical_gain <- tracks %>%
                # filter tracks to the selected date
                filter(date(track_timestamp) == input$date) %>%
                # get the elevation change from point to point and get a rounded time
                # this is the current rows elevation minus the last rows elevation
                mutate(elevation_change = elevation - lag(elevation),
                       track_timestamp_15s = round_date(track_timestamp, unit = "15s")) %>%
                # group by 15s intervals
                group_by(track_timestamp_15s) %>%
                # drop na's
                tidyr::drop_na(elevation_change) %>%
                # get the mean elevation change for each 15s window
                summarize(vertical_gain_15s = mean(elevation_change)) %>%
                # sum all positve elevation changes
                summarize(vertical_gain = sum(vertical_gain_15s[vertical_gain_15s > 0]))
            
            # use HTML to display the session stats
            div(class = "summary-stats",
                h3("Session Stats"),
                # the summary stats will be written in text with some special formatting and icons
                # this div contains the first column of session stats
                div(class = "col-sm-6",
                    p(class = "stats", sprintf("%.2f", summary_stats$total_distance),
                      icon("ruler", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Total Miles"),
                    br(),
                    p(class = "stats", sprintf("%.1f", summary_stats$avg_speed),
                      icon("tachometer-alt", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Avg Mph"),
                    br(),
                    p(class = "stats", sprintf("%.0f", summary_stats$max_elevation),
                      icon("mountain", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Max Elev (ft)")
                ),
                # the second column of session stats
                div(class = "col-sm-6",
                    p(class = "stats",
                      sprintf("%02d:%02d", minute(summary_stats$total_runtime),
                              second(summary_stats$total_runtime)),
                      icon("stopwatch", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Runtime (min)"),
                    br(),
                    p(class = "stats",
                      sprintf("%02d:%02d", minute(summary_stats$pace),
                              second(summary_stats$pace)),
                      icon("bolt", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Pace (min/mi)"),
                    br(),
                    p(class = "stats", sprintf("%.0f", vertical_gain$vertical_gain),
                      icon("level-up-alt", lib = "font-awesome")),
                    p(class = "stats-descriptor", "Vertical Gain (ft)")
                )
            )
        }
    })
})
