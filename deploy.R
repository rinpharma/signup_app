library(rsconnect)

# Print a list of app dependencies. Libraries need to be loaded
# before publishing so deployApp() knows what is necessary.
library(shiny)
library(tidyverse)
library(googlesheets)
library(shinythemes)
library(shinyWidgets)

# Set the account info for deployment.
setAccountInfo(name   = "epijimapps",
               token  = Sys.getenv("shinyapps_token"),
               secret = Sys.getenv("shinyapps_secret"))

# Deploy the application.
deployApp()
