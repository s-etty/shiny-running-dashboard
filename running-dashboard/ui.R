library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
    includeCSS("styles.css"),
    fluidRow(class = "header-navbar",
        column(4,
               p("sidebar expand button")),
        column(4, id = "page-title",
                h1(class = "header-navbar", "Run Dash")),
        column(4, id = "user-profile-header",
               p("user profile button"))
    ),
    fluidRow(class = "track-map",
             column(2, 
                    dateInput("date", label = h5("Select a Date"), 
                              value = "2020-08-27")),
                              #value = most_recent_date,
                              #min = min_date, max = most_recent_date,
                              #datesdisabled = disabled_dates$value)),
             #need to figure out how to disable all dates except those specified
             column(10, 
                    leafletOutput("mymap"))
                    
    ),
    fluidRow(class = "track-stats",
             column(8,
                    plotlyOutput("elevation"))
    )
))