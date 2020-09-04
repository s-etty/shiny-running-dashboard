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
                    uiOutput("date_input")),
             #need to figure out how to disable all dates except those specified
             column(10, 
                    leafletOutput("mymap"))
                    
    ),
    hr(),
    fluidRow(class = "track-stats",
             column(8,
                    plotlyOutput("elevation_speed")),
             column(4,
                    htmlOutput("current_summary_stats"))
    )
))