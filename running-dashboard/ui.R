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
    # include a style sheet for custom colors and styling
    includeCSS("styles.css"),
    
    # the code below is not complete and represents future upgrades to the
    # dashboard. It is under construction.
    ############################################
    # add a row for the header navbar. this will include user buttons, settings, and info links
    # fluidRow(class = "header-navbar",
    #          column(4,
    #                 p("sidebar expand button (coming soon)")),
    #          column(4, id = "page-title",
    #                 h1(class = "header-navbar", "Run Dash")),
    #          column(4, id = "user-profile-header",
    #                 p("user profile button (coming soon)"))
    # ),
    ############################################
    
    # add a row for the title of the page. will be upgraded to a navbar later on.
    ############################################
    fluidRow(class = "header-navbar",
             column(12, id = "page-title",
                    h1(class = "header-navbar", "Run Dash"))),
    ############################################
    
    # add a row for the calendar and map. these items link to the date_input and mymap
    # objects from the server.R file
    # the calender takes up 2/12 parts of the screen
    # the map takes up 10/12 parts of the screen
    ############################################
    fluidRow(class = "track-map",
             column(2, 
                    uiOutput("date_input")),
             column(10, 
                    leafletOutput("mymap"))
             
    ),
    ############################################
    
    # add a horizontal rule aka a line
    hr(),
    
    # add a row for the stats and elevation and speed plots
    # these items link to the current_summary_stats and elevation_speed outputs from the server.R
    # file.
    # stats takes up 4/12 parts of the screen
    # elevation and speed charts take up 8/12 parts of the screen
    ############################################
    fluidRow(class = "track-stats",
             column(4,
                    htmlOutput("current_summary_stats")),
             column(8,
                    plotlyOutput("elevation_speed"))

    )
    ############################################
    
    # the code below is not complete and represents future upgrades to the
    # dashboard. It is under construction.
    ############################################
    # below is code to add lifetime stats, e.g., number of miles run in the last week, month, year, 
    # lifetime.
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
    ############################################
))