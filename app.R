#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries
library(shiny)

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui, server)

## TODO
# other data
# speed up?

## PHREE asks


# library(rsconnect)
# rsconnect::deployApp("~/covid/covid-local-epi")

# profvis({
#   runApp()
# })



