# THIS IS THE VERSION THAT DEPLOYS TO SHINYAPPS.io
library(rlang)
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
# rsconnect::setAccountInfo(name='allingtonlab', 
   #                       token='20FB3DB97DEDF0C6A01EE094FF959E48', 
    #                      secret='Xck6O3lqhzWL2xD6BQKhSEbabiLiIaC16TD3eewq')


gs4_auth(path = "nypollinators-ba8dc68e43a5.json")
#gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")


sheet_id <- "https://docs.google.com/spreadsheets/d/1i4GgeNNNyl9zKKzhKfvoYChdTlrl_AHzKt_q0farXtc/edit?gid=0#gid=0"
#sheet_id<- "https://docs.google.com/spreadsheets/d/1Or4p-1j4T8hidApszksb2LbLrrQPGoHgnljoqx9cGXw/edit?gid=0#gid=0"
# the fields need to match the google sheet column headers AND the input IDs
fields <- c("who_entered",
            "setup_names",
            "collect_names",
            "site_ID",	
            "plot_ID",
             "set_date",	
             "collect_date",
            "start_date_time",	
	          "end_date_time",	
           # "sample_effort_hr",
            "num_traps_set",	
	          "num_traps_collect",	
             "sky",
            "wind",
            "temp_min",
            "temp_max",
            "notes")

people <- list("GA",
               "AF",
               "RW",
               "NV",
               "DM",
               "TS",
               "EM",
               "HS")

sites <- list("McG_",
              "MtP_",
              "Arnot")

plotIDs <- list("WT",
                "L",
                "S",
                "A",
                "B",
                "C",
                "D",
                "C1",  # what do we call the control plots at Arnot?
                "SW1",
                "SW2",
                "C2")

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
shinyApp(
  ui <- fluidPage(
    DT::dataTableOutput("entries", width = 300), tags$hr(),
    titlePanel("Pan Trap Data Entry"),
    selectInput("who_entered", "Name of person entering data",
                choices = people,
                selected = ""),
    textInput("setup_names", "Who Did Setup? Name(s)", ""),
    textInput("collect_names", "Who Did Collection? Name(s)", ""),
    selectInput("site_ID", "Site ID" ,
                choices = sites,
                selected = ""),
    selectInput("plot_ID", "Plot ID", 
                choices = plotIDs,
                selected = ""),
    dateInput("set_date", "Setup Date", "2025-04-01", format = "yyyy/mm/dd"),
    dateInput("collect_date", "Collection Date", "2025-04-01", format = "yyyy/mm/dd"),
    timeInput("start_date_time", "Enter time (5 minute steps)", 
              value = strptime("09:00:", "%T"), minute.steps = 5),
    timeInput("end_date_time", "Enter time (5 minute steps)", 
              value = strptime("09:00:", "%T"), minute.steps = 5),
    numericInput("num_traps_set", "Number of bowls set",value = 15, 
                 min = 1, 
                 max = 15 
                ), 
    numericInput("num_traps_collect", "Number of bowls collected",value = 15, 
                 min = 1, 
                 max = 15 
                  ),
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
    sliderInput("temp_min", "Minimum temp over past 24 hrs",
                min = 30, max = 100,
                value = 30),
    sliderInput("temp_max", "Maximum temp over past 24 hrs",
                min = 30, max = 100,
                value = 30),
    textInput("notes", "Notes", ""),
    actionButton("submit", "Submit")
  ),
  
  # Define server logic ----
  server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- eventReactive(input$submit, {
      data <- sapply(fields, function(x) input[[x]] %||% NA)
      data <- as.list(data)
      data <- as.data.frame(data, stringsAsFactors = FALSE)
      
      # Optional: ensure numeric conversion if needed
      data$Temp_min <- as.numeric(data$Temp_min)
      data$Temp_max <- as.numeric(data$Temp_max)
      
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
)


# test Run the app locally----
shinyApp(ui = ui, server = server)
runApp(appDir = "./", display.mode="showcase")

# to push the app to Shiny.io to share with others run this in the Console-do not save in teh app!:

#rsconnect::deployApp('.')





#rsconnect::deployApp('path/to/your/app')
# NOTE: When you deploy the console will yield a warning message about uid values replaces as 'nobody' user. this is ok.

