############################## Fruit SET NY ############################
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
#testing this template format. not sure if it works yet - 27 May 2026


# ---- AUTH ----
### Before running the app copy the following lines of code into the console
#setwd('/Users/gra38/Library/CloudStorage/Box-Box/Repositories/NYPollinatorData/NY_fruit_set')
#rsconnect::setAccountInfo(name='allingtonlab', token='20FB3DB97DEDF0C6A01EE094FF959E48', secret='Xck6O3lqhzWL2xD6BQKhSEbabiLiIaC16TD3eewq')

# gs4_auth(email = "your@email.edu", cache = ".secrets")
# Make sure to update your .gitignore to include .secrets and */.secrets
# You will be taken to an authorization page, make sure to check the box that allows for editing
###

gs4_auth(cache = ".secrets", email = "allingtonlab@gmail.com")
#gs4_auth(path = "nypollinators-ba8dc68e43a5.json")
# Add diagnostics
cat("Auth status:", gs4_has_token(), "\n")

###---- The settings:

sheet_id <- "1rL8C70uCAzGiOzTVlmTDKTAvfbsI-YvOzztMnaS3cRQ"

# the fields need to match the google sheet column headers AND the input IDs
fields <- c(  "observer",
              "date",
              "site",
              "farmID",
              "treeID",
              "trtCode",
              "fruitNum",
              "note")

observers <- list("Ginger",
                    "Rachel",
                    "Julie",
                    "Nolan",
                     "Tiffany",
                    "Sarvesh",
                  "Dylan")

farm <- list("BLKD1",
             "BLKD2",
             "POSY1",
             "POSY2",
             "CALD",
             "KRCH",
             "WHIT",
             "BARB",
             "HOLL",
             "CONH",
             "YOND",
             "KLOK",
             "MAYN",
             "CARA",
             "RR",
             "ONT2",
             "ONT3"
) ###


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
  selectInput("farmID", "Farm ID",
              choices = farm,
              selected = ""),
  dateInput("date", "Collection Date", "2025-06-01", format = "dd-mm-yyyy"),
  selectInput("site", "Farm Code",
              choices = farm,
              selected = ""),
  selectInput("treeID", "Tree ID",
              choices = tree_ID,
              selected = ""),
  selectInput("trtCode", "Treatment code",
              choices = trt,
              selected = ""),
  numericInput("fruitNum", "Fruit number", 
               value = 1,
               min = 0,
               max = 7),
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




####--------I tried this but it wouldn't run:
# # ---- GENERIC FORM BUILDER ----
# build_form_ui <- function(id, fields, title = "NY Fruit set data") {
#   ns <- NS(id)
#   
#   ui_inputs <- lapply(fields, function(f) {
#     switch(f$type,
#            text = textInput(ns(f$id), f$label),
#            numeric = numericInput(ns(f$id), f$label, value = NA),
#            date = dateInput(ns(f$id), f$label, value = Sys.Date()),
#            select = selectInput(ns(f$id), f$label, choices = f$choices),
#            stop(paste("Unknown field type:", f$type))
#     )
#   })
#   
#   tagList(
#     h3(title),
#     ui_inputs,
#     actionButton(ns("submit"), "Submit"),
#     br(), br(),
#     textOutput(ns("status"))
#   )
# }
# 
# # ---- GENERIC SERVER LOGIC ----
# form_server <- function(id, fields, sheet_id, sheet_name = 1) {
#   moduleServer(id, function(input, output, session) {
#     
#     observeEvent(input$submit, {
#       
#       # Build row from inputs
#       new_row <- lapply(fields, function(f) {
#         input[[f$id]]
#       })
#       
#       names(new_row) <- sapply(fields, function(f) f$id)
#       new_row <- as.data.frame(new_row)
#       
#       # Append to Google Sheet
#       sheet_append(sheet_id, new_row, sheet = sheet_name)
#       
#       output$status <- renderText("Submitted successfully!")
#       
#       # Reset inputs
#         lapply(fields, function(f) {
#           if (f$type == "text") {
#             updateTextInput(session, f$id, value = "")
#           } else if (f$type == "numeric") {
#             updateNumericInput(session, f$id, value = NA)
#           } else if (f$type == "date") {
#             updateDateInput(session, f$id, value = Sys.Date())
#           } else if (f$type == "select") {
#             updateSelectInput(session, f$id, selected = f$choices[1])
#           }
#         })
#       })
#     })
# }
# 
# # =====================================================
# # 🔵 DEFINE A DATASHEET HERE (THIS IS ALL YOU CHANGE)
# # =====================================================
# 
# datasheet_fields <- list(
#   list(id = "observer", type = "text", label = "Observer"),
#   list(id = "date", type = "date", label = "Date"),
#   list(id = "farmID", type = "select", label = "Farm_ID",
#        choices = c("BLKD1", "BLKD2", "CALD")),
#   list(id = "treeID", type = "select", label = "Tree_ID",
#        choices = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10")),
#   list(id = "trtCode", type = "select", label = "Trt_code",
#      choices = c("EA", "EB", "OH", "OP")),
#   list(id = "fruitNum", type = "numeric", label = "Fruit_number"),  #needto update to allow for NA
#   list(id = "note", type = "text", label = "Notes")
# )
# 
# 
# 
# SHEET_ID <- "1rL8C70uCAzGiOzTVlmTDKTAvfbsI-YvOzztMnaS3cRQ"
# 
# # ---- APP ----
# ui <- fluidPage(
#   build_form_ui("form1", datasheet_fields, title = "FruitSet")
# )
# 
# server <- function(input, output, session) {
#   form_server("form1", datasheet_fields, SHEET_ID)
# }
# 
# shinyApp(ui, server)
