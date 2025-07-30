# FRUIT QUALITY DATA INPUT

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
#setwd('/Users/gra38/Library/CloudStorage/Box-Box/Repositories/NYPollinatorData/UK_fruit_quality')
#rsconnect::setAccountInfo(name='allingtonlab', token='20FB3DB97DEDF0C6A01EE094FF959E48', secret='Xck6O3lqhzWL2xD6BQKhSEbabiLiIaC16TD3eewq')

# gs4_auth(email = "your@email.edu", cache = ".secrets")
# Make sure to update your .gitignore to include .secrets and */.secrets
# You will be taken to an authorization page, make sure to check the box that allows for editing
###

gs4_auth(cache = ".secrets", email = "allingtonlab@gmail.com")

#gs4_auth(path = "nypollinators-ba8dc68e43a5.json")
#gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")


sheet_id <- "1DM86zPtzrt0quj4GIsNeJUjK1DstfRGBpUDvQJJulA0"

# the fields need to match the google sheet column headers AND the input IDs
fields <- c(  "Farm_ID",
              "Date",
              "Field_staff",
              "Tree_ID",
              "Trt_code",
              "fruit_num",
              "size",
              "weight",
              "num_seeds",
              "Notes")

field_staff <- list("Lakshman",
               "Pankaj",
               "Pawan",
               "Rahul",
               "Other")

farm <- list("BAN1",
              "BAN2",
              "BUD1",
              "DUB1",
              "DUB2",
              "DUB3",
              "DUB4",
              "DUB5",
              "DUB6",
              "GAR1",
              "GAR2",
              "GAR3",
              "GAR4",
              "GAR5",
              "GAR6",
              "HAR1",
              "HAR2",
              "HAR3",
              "HAR4",
              "HAR5",
              "HAR6",
              "HAR7",
              "HAR8",
              "HAR9",
              "MYO1",
              "MYO2",
              "MYO3",
              "MYO4",
              "MYO5",
              "PAT1",
              "PAT2",
              "PAT3",
              "PAT4",
              "PAT5",
              "PAT6",
              "PAT7",
              "PAT8",
              "SAD1",
              "SAD2",
              "SAD3",
              "SAT1",
              "SAT2",
              "SAT3",
              "SAT4",
              "SAT5",
              "SIM1",
              "SUN1",
              "SUN2",
              "SUN3",
              "SUN4",
              "SUP1",
              "SUP2",
              "SUP3",
              "SUP4",
              "SUP5",
              "SUP6",
              "SUP7",
              "SUP8",
              "SUP9",
              "SUP10"
              ) # UK FARM

tree_ID <- list("T1",
                "T2",
                "T3",
                "T4",
                "T5",
                "T6",
                "T7",
                "T8",
                "T9",
                "T10"
                )

trt <- list("OH", #Trts
              "OP",
            "EA",
            "EB")

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
    titlePanel("Fruit Quality Data Entry"), #
    selectInput("Farm_ID", "Farm ID",
                choices = farm,
                selected = ""),
    dateInput("Date", "Collection Date", "2025-06-01", format = "dd-mm-yyyy"),
    selectInput("Field_staff", "Field Staff Name",
                choices = field_staff,
                selected = ""),
    selectInput("Tree_ID", "Tree ID",
                choices = tree_ID,
                selected = ""),
    selectInput("Trt_code", "Treatment code",
                choices = trt,
                selected = ""),
    numericInput("fruit_num", "Fruit number", 
                 value = 1,
                 min = 0,
                 max = 7),
    numericInput("size", "Size of fruit", 
                 value= 0,
                 min= 0,
                 max = 10
                 ),
    numericInput("weight", "Weight of fruit",
                 value = 0,
                 min= 0,
                 max = 150),
    numericInput("num_seeds", "Numer of seeds",
                 value = 0,
                 min= 0,
                 max = 20),
    textInput("notes", "Write YES here if there is info in the Notes field", ""),
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
