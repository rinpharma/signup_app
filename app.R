#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(googlesheets)

# gs_auth()

## Database connect ------

## Database functions ------------

table <- "RinPharmaRegistered"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}

### Load invites

invited <- read_csv(
    "invites.csv",
    col_types = cols(
      Type = col_character(),
      Name = col_character(),
      Email = col_character(),
      Company = col_character()
    )
  ) %>%
  mutate(
    Email = tolower(Email)
  )

## Form maintenance -------

  # Define the fields we want to save from the form
  fields <- c("entered_email", "industry", "confirmed")

### UI ----------

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("R/Pharma registration"),

   # Their email
   textInput("entered_email", "Please enter your email address", ""),

   # Industry
   radioButtons(
     "industry",
     "Please choose the research type that best represents you:",
      c("Regulatory body",
        "Pharma company",
        "Contract Research Organisation",
        "Non-profit",
        "Academic",
        "Student")),
   helpText("Spaces are very limited, so please only register if you plan to attend"),
   checkboxInput(
     "confirmed",
     "I confirm that I plan to attend R/Pharma on August 15th and 16th",
     FALSE),
   actionButton("submit", "Submit"),

   hr(),

   textOutput("response")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Whenever a form is submitted aggregate all form data
  formData <- eventReactive(input$submit,{

    withProgress(message = 'Processing registration', value = 0, {

    incProgress(0.2, detail = paste("Running input checks"))

    # check there is an email @
    validate(
      need(grep("@",input$entered_email) == 1,
           "Please enter an email address")
    )

    # check if invited
      validate(
        need(tolower(input$entered_email) %in% tolower(invited$Email),
             "Sorry, your email is not on the list of invited people")
      )

    # check they agree
      validate(
        need(input$confirmed,
             "Unfortunately, we can only confirm tickets for people that can confirm they will attend")
      )

      incProgress(0.4, detail = paste("Checking if you are already registered"))

    # download current sheet
      old_data <- loadData()

    # check if they are already registered
    validate(
      need(!tolower(input$entered_email) %in% tolower(old_data$Email),
           paste(input$entered_email,"is already registered."))
    )

    # Merge with data
      data <- data.frame(
        Email = tolower(input$entered_email),
        Industry = input$industry,
        Attending = input$confirmed,
        Time = Sys.time(),
        stringsAsFactors = FALSE
      ) %>%
      left_join(
        invited,
        by = "Email"
      ) %>%
      select(
        Name,Email,Time,Type,Industry,Company,Attending
      )
    })
    data
  })

  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    tosave <- formData()
    withProgress(message = 'Saving registration', value = 1, {
      saveData(tosave)
    })
  })

  output$response <- renderText({
    input$submit
    output <- formData()
    paste0(
      output$Name,", thank you for registering."
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

