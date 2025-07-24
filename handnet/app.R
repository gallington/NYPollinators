# HAND NETTING DATA INPUT

library(shiny)
library(shinyFeedback)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(rsconnect)
library(DT)
library(lubridate)
library(shinyTime)
library(rsconnect)


### Before running the app copy the following lines of code into the console
#setwd('/Users/gra38/Library/CloudStorage/Box-Box/Repositories/NYPollinatorData/handnet')
#rsconnect::setAccountInfo(name='allingtonlab', token='20FB3DB97DEDF0C6A01EE094FF959E48', secret='Xck6O3lqhzWL2xD6BQKhSEbabiLiIaC16TD3eewq')

# gs4_auth(email = "your@email.edu", cache = ".secrets")
# Make sure to update your .gitignore to include .secrets and */.secrets
# You will be taken to an authorization page, make sure to check the box that allows for editing
###

gs4_auth(cache = ".secrets", email = "allingtonlab@gmail.com")

#gs4_auth(path = "nypollinators-ba8dc68e43a5.json")
#gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")


sheet_id <- "1YG5vfcMk4W27fCPs1SVCI-gjJbhuUca_hAPB3s0rdis"

# the fields need to match the google sheet column headers AND the input IDs
fields <- c(  "who_entered",
              "num_collectors",
              "collector1",
              "collector2",
              "collect_date",
              "collect_time",
              "site_ID",
              "plot_ID",
              "sky",
              "wind",
              "notes")

people <- list("GA",
               "AF",
               "RW",
               "NV",
               "DM",
               "TS",
               "EM",
               "HS",
               "N/A")

sites <- list("McG",
              "MtP7C",
              "MtP11",
              "Arn6-4",
              "Arn3-1",
              "Arn6-6",
              "Arn6-9",
              "Arn6-2")

plotIDs <- list("WT",
                "L",
                "S",
                "A",
                "B",
                "C",
                "D",
                "SW5",
                "SW16",
                "SW22",
                "C8",
                "C15",
                "C21",
                "6-2"
                )

# Define functions to use in server logic
table <- "entries"


saveData <- function(data) {
  # Assumes `data` is already a list or data frame of values from inputs
  data <- data %>% as.list() %>% data.frame()
  sheet_append(sheet_id, data)
}

loadData <- function() {
  # Read the data
  read_sheet(sheet_id)
}


# Define UI for app that can append to a google sheet  from input options
   ui <- fluidPage(
    DT::dataTableOutput("entries", width = 300), tags$hr(),
    titlePanel("Hand Net Data Entry"),
    selectInput("who_entered", "Name of person entering data",
                choices = people,
                selected = ""),
    numericInput("num_collectors", "Number of people netting?",value = 2,
                 min = 1,
                 max = 5),
    selectInput("collector1", "Who Was Netting? Name 1:",
                choices = people,
                selected = ""),
    selectInput("collector2", "Who Was Netting? Name 2: \n If more than 2 just enter 2 names. If less that 2 enter N/A here.",
                choices = people,
                selected = ""),
    dateInput("collect_date", "Collection Date", "2025-04-01", format = "yyyy/mm/dd"),
    timeInput("collect_time", "Time:",
              value = strptime("09:00:00", "%T"), minute.steps = 5),
    selectInput("site_ID", "Site ID" ,
                choices = sites,
                selected = ""),
    selectInput("plot_ID", "Plot ID",
                choices = plotIDs,
                selected = ""),
    selectInput("sky", "Weather when pan traps were out",
                choices = list("Sunny",
                               "Partly Sunny",
                               "Cloudy"),
                selected = ""),
    selectInput("wind", "Wind conditions when pan traps were out",
                choices = list("No wind",
                               "Breezy",
                               "Windy"),
                selected = ""),
    textInput("notes", "Notes", ""),
    actionButton("submit", "Submit")
  )
  
  # Define server logic ----
  server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- eventReactive(input$submit, {
      data <- sapply(fields, function(x) {
        val <- input[[x]]
        if (is.null(val)) NA else val
      }, simplify = FALSE)
      data <- as.data.frame(data, stringsAsFactors = FALSE)
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
  
    # Show the previous entries
    # (update with current entry when Submit is clicked)
    output$entries <- DT::renderDataTable({
      req(input$submit)     # makes sure submit was clicked
      loadData()            # returns a data frame
    })
  }



# test Run the app locally----
shinyApp(ui = ui, server = server)


# NOTE: When you deploy the console will yield a warning message about uid values replaces as 'nobody' user. this is ok.
