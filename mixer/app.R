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
library(magrittr)
library(googlesheets)

# gs_auth()

## Database connect ------

## Database functions ------------

  table <- "RinPharmaMixer"

  saveData <- function(data,table) {
    # Grab the Google Sheet
    sheet <- gs_title(table)
    # Add the data as a new row
    gs_add_row(sheet, input = data)
  }

  loadData <- function(table) {
    # Grab the Google Sheet
    sheet <- gs_title(table)
    # Read the data
    gs_read_csv(sheet)
  }

### Load coming

  sheet <- gs_title("RinPharmaRegistered")
  # Read the data
  registered <-  gs_read_csv(sheet) %>%
    filter(Name != "test")

### UI ----------

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("R/Pharma workshop and mixer registration"),

  sidebarPanel(

    # Their email
    textInput("entered_email", "Please enter the email address we used to contact you", ""),

    helpText("Come join fellow R/Pharma Conference attendees for an informal mixer hosted by Metrum Research Group following our day one programming!"),
    helpText("Time: Wednesday August 15th from 6:00-7:30pm."),

    radioButtons("confirmed", "Do you plan to attend the mixer?",
                 c("Yes" = "Yes",
                   "No" = "No")
                 ),
    hr(),
    radioButtons("SessionOne", "Would you like to sign up for an 8am workshop on Wednesday (Aug 15th)?",
                 c("No" = "No",
                   "Keeping things Peachy when Shiny gets Hairy" = "Foos",
                   "Analyzing Clinical Trials Data with R Adrian Waddell" = "Waddell",
                   "Bayesian Models for Smaller Trial Sizes - Stan with R for analysis" = "Lee",
                   "Moving Fast Without Breaking Things: Navigating the R Ecosystem in an Enterprise Environment" = "Pastoor"
                   )
    ),
    hr(),
    radioButtons("SessionTwo", "Would you like to sign up for an 8am workshop on Thursday (Aug 16th)?",
                 c("No" = "No",
                   "Interactive data visualization with R, plotly, and dashR" = "Sievert",
                   "The Challenges of Validating R" = "Nicholls",
                   " The largest Shiny application in the world. Roche.Diagnostics.bioWARP" = "Wolf"
                 )
    ),
    actionButton("submit", "Submit"),

    hr(),

    textOutput("response"),
    width = 6
  ),
  mainPanel(
  h3("Summary of open spots"),
  helpText("This table will not update when you press submit"),
  tableOutput("table"),width = 6
)
)

# Define server logic
server <- function(input, output) {

  old_data <- reactive({
    ### previous replies
    sheet <- gs_title("RinPharmaMixer")
    # Read the data
    old_data <-  gs_read_csv(sheet) %>%
      filter(Name != "test")

    old_data
  })


  the_table <- reactive({
    tibble(
    Workshop = c(
      "The largest Shiny application in the world. Roche.Diagnostics.bioWARP",
      "The Challenges of Validating R",
      "Interactive data visualization with R, plotly, and dashR",
      "Keeping things Peachy when Shiny gets Hairy",
      "Analyzing Clinical Trials Data with R",
      "Bayesian Models for Smaller Trial Sizes - Stan with R for analysis.",
      "Moving Fast Without Breaking Things: Navigating the R Ecosystem in an Enterprise Environment"
    ),
    Lead = c("Wolf","Nicholls","Sievert","Foos","Waddell","Lee","Pastoor"),
    Spaces = c(20,45,60,45,60,20,30)
  ) %>%
    left_join(
      rbind(
        old_data() %>%
          select(Name,Email,Lead = SessionOne),
        old_data() %>%
          select(Name,Email,Lead = SessionTwo)
      ) %>%
        group_by(Lead) %>%
        summarise(Attending = n()),
      by = "Lead"
    ) %>%
    mutate(
      Attending = ifelse(is.na(Attending),0,Attending),
      Attending = as.integer(Attending),
      `Spaces left` = as.integer(Spaces - Attending)
    ) %>%
    select(Workshop,Attending,`Spaces left`,Lead)
  })

  full_workshops <- reactive({
    the_table() %>%
      filter(`Spaces left` == 0) %$%
      Lead
  })

  output$table <- renderTable({
    the_table() %>% select(-Lead)
  })

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
        need(tolower(input$entered_email) %in% tolower(registered$Email),
             "Sorry, your email is not on the list of registered people, please use the form at rinpharma.com to get in contact.")
      )

      incProgress(0.2, detail = paste("Checking if there is space in the workshops"))

      # check if workshop full
      validate(
        need(!c(input$SessionOne,input$SessionTwo) %in% full_workshops(),
             "Sorry, one of the workshops you selected is full.")
      )

      incProgress(0.2, detail = paste("Checking if you are already registered"))

      # check if they are already registered
      validate(
        need(!tolower(input$entered_email) %in% tolower(old_data()$Email),
             paste(input$entered_email,"is already registered. See you soon in Boston."))
      )

      validate(
        need(input$submit == 1,
             paste(input$entered_email,"you already pressed submit once. Please reload the app if you got an error the first time."))
      )

      # Merge with data
      data <- data.frame(
        Email = tolower(input$entered_email),
        Industry = "POST REMOVAL",
        Attending = input$confirmed,
        Time = Sys.time(),
        SessionOne = input$SessionOne,
        SessionTwo = input$SessionTwo,
        stringsAsFactors = FALSE
      ) %>%
        left_join(
          registered %>% select(Name, Email),
          by = "Email"
        ) %>%
        select(
          Name,Email,Time,Attending,SessionOne,SessionTwo
        )
    })
    data
  })

  # When the Submit button is clicked, save the form data
  observeEvent(input$submit,{
    # check if they are already registered
    tosave <- formData()
    withProgress(message = 'Saving registration', value = 1, {
      saveData(tosave,"RinPharmaMixer")
    })
  })

  output$response <- renderText({
    input$submit
    output <- formData()
    paste0(
      output$Name,", thank you for registering. See you in Boston."
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

