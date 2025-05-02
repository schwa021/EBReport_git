# Load required libraries
library(shiny)
library(quarto)

# Define UI -----
ui <- fluidPage(
  
  # UI elements for parameter input
  textInput("MRN", "MRN", "606085"),
  textInput("Event_Date", "Event Date", "2018-06-21"),
  textInput("username", "User Name", "mschwartz@gillettechildrens.com"),
  passwordInput("password", "Password", "Duffy001!))))))))"),
  checkboxInput("deid", "Deidentify?", value = FALSE),
  checkboxInput("usevideo", "Include Video?", value = TRUE),
  checkboxInput("figsave", "Save Figures?", value = FALSE),
  checkboxInput("usegait", "Use Gait Data?", value = TRUE),
  checkboxInput("custom_outcome", "Use Custom Outcome?", value = FALSE),
  
  # Button to trigger Quarto rendering
  actionButton("renderBtn", "Render EB GAIT Report")
)

# Define server ----
server <- function(input, output, session) {
  
  # Function to render Quarto document ----
  renderQuarto <- function() {
    
    # EBReport Template ----
    EBReport_params <- list(
      MRN = input$MRN,
      Event_Date = input$Event_Date,
      deid = input$deid,
      usevideo = input$usevideo,
      figsave = input$figsave,
      usegait = input$usegait,
      custom_outcome = input$custom_outcome,
      username = input$username,
      password = input$password
    )
    
    EBReport_script <- "EBReport.qmd"
    EBReport_outfile <- 
      glue::glue("EBReport_{EBReport_params$MRN}_{EBReport_params$Event_Date}.html")
    
    quarto::quarto_render(
      EBReport_script, 
      execute_params = EBReport_params, 
      output_file = EBReport_outfile)
    
    # EBCompare Template ----
    EBCompare_params <- list(
      MRN = input$MRN,
      Event_Date = input$Event_Date,
      fit_prop = FALSE,
      fit_out = FALSE
    )
    
    EBCompare_script <- "EBCompare.qmd"
    EBCompare_outfile <- 
      glue::glue("EBCompare_{EBCompare_params$MRN}_{EBCompare_params$Event_Date}.html")
    
    quarto::quarto_render(
      EBCompare_script,
      execute_params = EBCompare_params,
      output_file = EBCompare_outfile)
  }
  
  # Event handler for rendering Quarto document...
  observeEvent(input$renderBtn, {renderQuarto()})
  
}

# Run the Shiny app -----
shinyApp(ui, server)
