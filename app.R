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
library(shinythemes)

# to create .httr-oauth in rds format
# token <- gs_auth()
# saveRDS(token, file = "googlesheets_token.rds")
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))

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
    gs_read(ws = "App_Workshops") %>%
    mutate(
      Workshop = paste0(
        `Workshop Leader`,": ",`Workshop Title`
      ),
      Time = paste(`Start Time`,"to",`End Time`)
    ) %>%
    select(
      Workshop, Capacity, Time
    )

  workshop_times <- unique(workshop_sizes$Time)

  if(length(workshop_times) != 2) stop("Number of workshops changed")

  options_workshop_morning <- workshop_sizes %>%
    filter(Time == workshop_times[1]) %>%
    pull(Workshop)

  options_workshop_afternoon <- workshop_sizes %>%
    filter(Time == workshop_times[2]) %>%
    pull(Workshop)


saveData <- function(data) {
  # Add the data as a new row
  gs_add_row(ss = workbook,ws = "App_Output", input = data)
}


  check_invitations <- function() {
    # Grab the Google Sheet
    workbook %>%
      gs_read(ws = "App_Output")
  }

  # download current sheet
  old_data <- check_invitations()

### UI ----------

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # GOOGLE ANALYTICS
  tags$head(includeScript("gs.js")),

   # Application title
   titlePanel("R/Pharma registration"),

  sidebarPanel(

   # Their email
    h2("DEVELOPMENT APP - dates/titles not confirmed and responses will be deleted"),

   textInput("entered_email", "Please enter the email address we used to contact you", ""),

   h2("Conference mixer"),

    helpText("Come join fellow R/Pharma Conference attendees for an informal mixer hosted by Metrum Research Group following our day one programming!"),
    helpText("Time: Thursday August 22nd from 6:00-7:30pm."),

    radioButtons("Mixer", "Do you plan to attend the mixer?",
                 c("Yes" = "Yes",
                   "No" = "No")
    ),

  h2("Conference workshops"),

  helpText("On the day before the conference (Wednesday 21st of August), there will be free morning and afternoon workshops."),

  radioButtons("MorningWorkshop", "Would you like to sign up for an morning workshop on Wednesday (Aug 21st)?",
               c("No",
                 options_workshop_morning
               )
  ),
  hr(),
  radioButtons("AfternoonWorkshop", "Would you like to sign up for an afternoon workshop on Wednesday (Aug 21st)?",
               c("No",
                 options_workshop_afternoon
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

  h2("Conference signup"),

  helpText(
    a("Please click here to view the terms and conditions",
      href = "http://rinpharma.com/terms")
    ),
  helpText("Spaces are very limited, so please only register if you plan to attend"),
   checkboxInput(
     "confirmed",
     "I confirm that I accept the terms linked to above and I plan to attend R/Pharma on August 22nd and 23rd.",
     FALSE),
   actionButton("submit", "Submit"),

   hr(),

   textOutput("response"),
  width = 6
  ),
  mainPanel(
    h3("Summary of open spots"),
    helpText("This table will not update when you press submit"),
    tableOutput("table"),
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

    # check if they are already registered
    validate(
      need(!tolower(input$entered_email) %in% tolower(old_data$Email),
           paste(input$entered_email,"is already registered. See you soon in Boston."))
    )

    # check if workshop full
    validate(
      need(!c(input$MorningWorkshop,input$AfternoonWorkshop) %in% full_workshops(),
           "Sorry, one of the workshops you selected is full.")
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

  ## Workshop code
  the_table <- reactive({
    workshop_sizes %>%
      left_join(
        rbind(
          old_data %>%
            select(Name,Email,Workshop = MorningWorkshop),
          old_data %>%
            select(Name,Email,Workshop = AfternoonWorkshop)
        ) %>%
          group_by(Workshop) %>%
          summarise(Attending = n()),
        by = "Workshop"
      ) %>%
      mutate(
        Attending = ifelse(is.na(Attending),0,Attending),
        Attending = as.integer(Attending),
        `Spaces left` = as.integer(Capacity - Attending)
      ) %>%
      select(Workshop,Attending,`Spaces left`)
  })

  full_workshops <- reactive({
    the_table() %>%
      filter(`Spaces left` == 0) %>%
      pull(Workshop)
  })

  output$table <- renderTable({
    the_table()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

