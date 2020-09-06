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
             column(4,
                    htmlOutput("current_summary_stats")),
             column(8,
                    plotlyOutput("elevation_speed"))

    ),
    fluidRow(class = "lifetime-stats",
            column(9,
                   fluidRow(class = "background-bar",
                            div()),
                   fluidRow(class = "lifetime-stats-container",
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),
                            p("here we are"),p("here we are"))),
            column(3, class = "lifetime-stats-filters",
                    h3("More Stats"),
                    selectInput("lifetime_filter",
                                label = "Select Time Window",
                                choices = c("Week", "Month", "Year", "Lifetime"),
                                selected = "Week")
                 )
            )
))