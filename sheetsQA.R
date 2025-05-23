# Here’s a starter R script for doing QA on data submitted via Google Forms. This script will:
#   1.	Read the Google Sheet linked to your form.
# 2.	Check for missing values.
# 3.	Flag unexpected responses.
# 4.	Convert and clean data types.
# 5.	Save a cleaned version for analysis or archival.



library(googlesheets4)
library(dplyr)
library(readr)
library(stringr)
library(janitor)

# 1. Authorize and read the sheet
# (You may need to go through a browser login the first time)
sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID_HERE"
raw_data <- read_sheet(sheet_url)

# 2. Clean column names
data <- raw_data %>%
  janitor::clean_names()

# 3. Basic QA checks

# Check for missing values
missing_report <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "field", values_to = "missing_count") %>%
  filter(missing_count > 0)

print("Missing values per field:")
print(missing_report)

# 4. Validate categorical fields
# Suppose 'species' should be one of a few known values
allowed_species <- c("Honeybee", "Bumblebee", "Other")

invalid_species <- data %>%
  filter(!species %in% allowed_species)

if (nrow(invalid_species) > 0) {
  warning("Some invalid species found:")
  print(unique(invalid_species$species))
}

# 5. Convert date/time columns
# Adjust field name as needed
data <- data %>%
  mutate(
    observation_date = as.Date(observation_date, format = "%Y-%m-%d"),
    start_time = parse_time(start_time, format = "%H:%M")
  )

# 6. Save cleaned version
write_csv(data, "cleaned_data.csv")

# 7. Optional: Save invalid rows for review
if (nrow(invalid_species) > 0) {
  write_csv(invalid_species, "flagged_invalid_species.csv")
}

message("QA complete. Cleaned data written to 'cleaned_data.csv'.")