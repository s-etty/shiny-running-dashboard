library(shiny)
library(leaflet)

shinyUI(fluidPage(
    theme = "styles.css",
    fluidRow(class = "header-navbar",
        column(4,
               p("sidebar expand button")),
        column(4, id = "page-title",
                h1(class = "header-navbar", "Run Dash")),
        column(4, id = "user-profile-header",
               p("user profile button"))
    ),
    fluidRow(class = "latest-track")
))