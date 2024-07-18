# Load required libraries
library(shiny)
library(quarto)

# Define UI -----
ui <- fluidPage(
  # UI elements for parameter input
  textInput("MRN", "MRN", "606085"),
  textInput("Event_Date", "Event Date", "2018-06-21"),
  checkboxInput("deid", "Deidentify?", value = FALSE),
  checkboxInput("usevideo", "Include Video?", value = TRUE),
  checkboxInput("figsave", "Save Figures?", value = FALSE),
  checkboxInput("usegait", "Use Gait Data?", value = TRUE),
  checkboxInput("custom_outcome", "Use Custom Outcome?", value = FALSE),
  
  # Button to trigger Quarto rendering
  actionButton("renderBtn", "Render EB GAIT Report")
)

# Define server -----
server <- function(input, output, session) {
  
  # Function to render Quarto document...
  renderQuarto <- function() {
    params <- list(
      MRN = input$MRN,
      Event_Date = input$Event_Date,
      deid = input$deid,
      usevideo = input$usevideo,
      figsave = input$figsave,
      usegait = input$usegait,
      custom_outcome = input$custom_outcome
    )
    
    # Template
    quarto_file <- "EBReport.qmd"
    output_file <- glue::glue("EBReport_{params$MRN}_{params$Event_Date}.html")
    
    # Render Quarto document with parameters...
    quarto::quarto_render(
      quarto_file, 
      execute_params = params, 
      output_file = output_file)
  }
  
  # Event handler for rendering Quarto document...
  observeEvent(input$renderBtn, {
    renderQuarto()
  })
  
}

# Run the Shiny app -----
shinyApp(ui, server)
