# Running Dashboard Using Shiny

The goal of this project is to learn how to create Shiny apps, deploy them using [shinyapps.io](https://www.shinyapps.io/), and see my running progress. You can find the app [here](https://setty.shinyapps.io/running-dashboard/).

See issues in this repo for things I'm still working on.

## Skills Used in this Repo

- Shiny
- Leaflet
- Plotly

## Notes About Data

The data for these runs can be found in the data directory. Each csv represents a separate run.

One other thing to note. For many of these runs, the GPS settings were not optimized in the app I was using to record the data. I didn't realize it until later, but there is a setting that allows the GPS recording app to keep track of your location in between each point. This gives it *much* better accuracy, but at the expense of battery. By default it's switched off.

## References, Resources, and Learning

Of course, as this is a new skill for me, I will be using a lot of resources to learn and build this app. This is a running log of all the things that helped get me unstuck on various problems. Maybe they'll be useful later on.

- [R Studio Tutorials](https://shiny.rstudio.com/tutorial/)
- [Reading Multiple CSV](https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/)
- [Creating Leaflet Maps in Shiny](https://rstudio.github.io/leaflet/shiny.html)
- [Working with Lat/Long Data](https://cran.r-project.org/web/packages/geosphere/geosphere.pdf)
- [Filtering by Date in Shiny](https://stackoverflow.com/questions/49848841/filtering-by-date-in-shiny)
