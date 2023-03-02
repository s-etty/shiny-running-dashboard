###########################################
#
# The code included in this file renders the ui for this shiny app.
# The app is used to show running statistics.
#
###########################################

library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
    includeCSS("styles.css"),
    #
    fluidRow(class = "header-navbar",
             column(4,
                    p("sidebar expand button (coming soon)")),
             column(4, id = "page-title",
                    h1(class = "header-navbar", "Run Dash")),
             column(4, id = "user-profile-header",
                    p("user profile button (coming soon)"))
    ),
    fluidRow(class = "track-map",
             column(2, 
                    uiOutput("date_input")),
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
    fluidRow(class = 'testing',
             column(12,
                    htmlOutput("timezone_test")))
    # fluidRow(class = "lifetime-stats",
    #          column(1),
    #          column(7,
    #                fluidRow(class = "lifetime-stats-container",
    #                         p("here we are"),
    #                         p("here we are"),
    #                         p("here we are")
    #                         )
    #         ),
    #         column(1),
    #         column(3, class = "lifetime-stats-filters",
    #                         selectInput("lifetime_filter",
    #                         label = "Select Time Window",
    #                         choices = c("Week", "Month", "Year", "Lifetime"),
    #                         selected = "Week")
    #              )
    # )
))