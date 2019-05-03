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

# to create .httr-oauth
# gs_auth()

# get key
# extract_key_from_url(
#  "https://docs.google.com/spreadsheets/d/1UxsynZg8U-FG_Yf_LiUNRAebtJKsUQpOrehPIfPqqw0/edit?usp=sharing"
# )
# > 1UxsynZg8U-FG_Yf_LiUNRAebtJKsUQpOrehPIfPqqw0

## Database connect ------

## Database functions ------------

  workbook_key <- "1UxsynZg8U-FG_Yf_LiUNRAebtJKsUQpOrehPIfPqqw0"
  workbook <- gs_key(workbook_key)

  # pull invitations
  invitations <- workbook %>%
    gs_read(ws = "App_Invitations") %>%
    mutate(
      Email = tolower(Email)
    )

  # pull workshop sizes
  workshop_sizes <- workbook %>%
    gs_read(ws = "App_Workshops")

  testdata2 <- workbook %>%
    gs_read(ws = "App_Output")

saveData <- function(data) {
  # Add the data as a new row
  gs_add_row(ss = workbook,ws = "App_Output", input = data)
}


  check_invitations <- function() {
    # Grab the Google Sheet
    sheet <- workbook %>%
      gs_read(ws = "App_Output")
  }

## Form maintenance -------

  # Define the fields we want to save from the form
  fields <- c("entered_email", "industry", "confirmed")

### UI ----------

# Define UI for application that draws a histogram
ui <- fluidPage(
  # GOOGLE ANALYTICS
  tags$head(includeScript("gs.js")),

   # Application title
   titlePanel("R/Pharma registration"),

  sidebarPanel(

   # Their email
   textInput("entered_email", "Please enter the email address we used to contact you", ""),

    helpText("Come join fellow R/Pharma Conference attendees for an informal mixer hosted by Metrum Research Group following our day one programming!"),
    helpText("Time: Wednesday August 15th from 6:00-7:30pm."),

    radioButtons("Mixer", "Do you plan to attend the mixer?",
                 c("Yes" = "Yes",
                   "No" = "No")
    ),
  radioButtons("MorningWorkshop", "Would you like to sign up for an 8am workshop on Wednesday (Aug 15th)?",
               c("No" = "No",
                 "Keeping things Peachy when Shiny gets Hairy" = "Foos",
                 "Analyzing Clinical Trials Data with R Adrian Waddell" = "Waddell",
                 "Bayesian Models for Smaller Trial Sizes - Stan with R for analysis" = "Lee",
                 "Moving Fast Without Breaking Things: Navigating the R Ecosystem in an Enterprise Environment" = "Pastoor"
               )
  ),
  hr(),
  radioButtons("AfternoonWorkshop", "Would you like to sign up for an 8am workshop on Thursday (Aug 16th)?",
               c("No" = "No",
                 "Interactive data visualization with R, plotly, and dashR" = "Sievert",
                 "The Challenges of Validating R" = "Nicholls",
                 " The largest Shiny application in the world. Roche.Diagnostics.bioWARP" = "Wolf"
               )
  ),
  hr(),

   # Industry
   # radioButtons(
   #   "industry",
   #   "Please choose the research type that best represents you:",
   #    c("Pharma company",
   #      "Regulatory body",
   #      "Contract Research Organisation",
   #      "Non-profit",
   #      "Academic",
   #      "Student")),
  helpText(
    a("Please click here to view the terms and conditions",
      href = "http://rinpharma.com/terms")
    ),
  helpText("Spaces are very limited, so please only register if you plan to attend"),
   checkboxInput(
     "confirmed",
     "I confirm that I accept the terms linked to above and I plan to attend R/Pharma on August 15th and 16th",
     FALSE),
   actionButton("submit", "Submit"),

   hr(),

   textOutput("response"),
  width = 6
  ),
  mainPanel(
    h3("Summary of open spots"),
    helpText("This table will not update when you press submit"),
    #tableOutput("table"),
    width = 6
  )

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

    # check if invitations
      validate(
        need(tolower(input$entered_email) %in% tolower(invitations$Email),
             "Sorry, your email is not on the list of invitations people, please use the form at rinpharma.com to get in contact.")
      )

    # check they agree
      validate(
        need(input$confirmed,
             "Unfortunately, we can only confirm tickets for people that can confirm they will attend")
      )

      incProgress(0.4, detail = paste("Checking if you are already registered"))

    # download current sheet
      old_data <- check_invitations()

    # check if they are already registered
    validate(
      need(!tolower(input$entered_email) %in% tolower(old_data$Email),
           paste(input$entered_email,"is already registered. See you soon in Boston."))
    )

    # Merge with data
      data <- data.frame(
        Email = tolower(input$entered_email),
        Attending = input$confirmed,
        Time = Sys.time(),
        Mixer = input$Mixer,
        MorningWorkshop = input$MorningWorkshop,
        AfternoonWorkshop = input$AfternoonWorkshop,
        stringsAsFactors = FALSE
      ) %>%
      left_join(
        invitations,
        by = "Email"
      ) %>%
      select(
        Name,Email,Company,Attending,Mixer,MorningWorkshop,AfternoonWorkshop
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
      output$Name,", thank you for registering. See you in Boston."
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

