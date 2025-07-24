# BASIC CODE TO PASTE INTO CONSOLE TO DEPLOY AN APP 
# Do this while you still have the working directory set to the relevant folder
# need to add in the basics for connecting to google sheets


# Load necessary package
library(rsconnect)

# Set path to your app folder and service account key
app_dir <- "./handnet/"  # adjust app folder accordingly
auth_file <- "nypollinators-ba8dc68e43a5.json"
appName <- "handnet"
shinyUser <- "allingtonlab"


# Make sure the file exists
if (!file.exists(file.path(app_dir, auth_file))) {
  stop("The authentication JSON file is missing!")
}

# Deploy the app with the required files
deployApp(
  appDir = app_dir,
  appFiles = c("app.R", auth_file),
  appName = appName,  # or change to your preferred name
  account = shinyUser  # replace with your shinyapps.io account name
)

# to push the app to Shiny.io to share with others deploy it here:

#rsconnect::deployApp(appFiles = c("app.R", "nypollinators-ba8dc68e43a5.json"))
#rsconnect::deployApp('.')