library(tidyverse)
library(glue)

# RUN POWER AUTOMATE SCRIPT #

#  Define the path to  OneDrive trigger folder
trigger_file <- "~/../OneDrive - Gillette Children's Specialty Healthcare/EBVerse/EBReport_git/Flow_Triggers/calendar_trigger.txt"

unique_id <- format(Sys.time(), "%H%M%S")
trigger_file <- glue("C:/Users/mschwartz/OneDrive - Gillette Children's Specialty Healthcare/EBVerse/EBReport_git/Flow_Triggers/run_{unique_id}.txt")
#  Trigger the Flow by writing a timestamp to the file
# message("🚀 Dropping signal file in OneDrive...")
if (file.exists(trigger_file)) {file.remove(trigger_file)} 
writeLines(as.character(Sys.time()), trigger_file)

# Wait for Power Automate and OneDrive Sync
# Since OneDrive sync isn't instant
message("⏳ Waiting 30 seconds for Power Automate to finish the CSV...")
Sys.sleep(30)


## FIND CASES READY FOR EB REPORT GENERATION ##

# Read the interpretation schedule
df <- read_csv("Interpretation_Schedule.csv")

# Initial filtering by date
df_filtered <- df %>%
  mutate(Date_of_Interp = as.Date(Date_of_Interp),
         Event_Date = as.Date(Event_Date)) %>%
  filter(Date_of_Interp >= Sys.Date())

# Define the destination folder for existing reports
report_dir <- "C:/Users/mschwartz/OneDrive - Gillette Children's Specialty Healthcare/EBVerse/EBReport_git/REPORTS"

# Check for existing files
df_final_to_generate <- df_filtered %>%
  mutate(
    # --- Check if printable exists --> data is ready ---
    date_suffix = format(Event_Date, "%b%Y"),
    source_path = paste0(
      "L:/Motion Analysis Lab/Electronic Report/",
      MRN,
      "/",
      MRN,
      date_suffix,
      "printable.pdf"
    ),
    data_is_ready = file.exists(source_path),
    
    # --- Check if Report ALREADY exists --> don't recreate ---
    report_filename = paste0("EBReport_", MRN, "_", format(Event_Date, "%Y-%m-%d"), ".html"),
    report_path = file.path(report_dir, report_filename),
    already_generated = file.exists(report_path)
  ) %>%
  
  # Keep rows to generate ---
  filter(data_is_ready == TRUE & already_generated == FALSE) %>%
  select(MRN, Date_of_Interp, MD_email, MD_name, Event_Date, FIN)

# View the reports that actually need to be generated
# print(df_final_to_generate)

# Prepare the parameters for report generation --- 
df_to_export <- df_final_to_generate %>%
  transmute(
    MRN = as.character(MRN),
    Event_Date = as.Date(Event_Date),
    deid = FALSE,
    usevideo = TRUE,
    compare = TRUE,
    figsave = FALSE,
    usegait = TRUE,
    custom_outcome = FALSE
  )

# Write to Excel
write_csv(df_to_export, file = "report_params.csv")

#  Clean up by removing calendar_trigger.txt
if (file.exists(trigger_file)) {file.remove(trigger_file)} 


