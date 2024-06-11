build_importanttbl <- function(goal, params){
  
  # Choose palette -----
  pal <-
    sequential_hcl(
      n = 3,
      h = 360,
      c = c(85, NA, NA),
      l = c(55, 90),
      power = 1.8
    )
  
  domain <- c("sev", "mod", "mil")
  
  sevdat <- tribble(
    ~Difficulty,                             ~sev,
    "Extremely Difficult / Impossible",      "sev",
    "Very Difficult",                        "sev",
    "Difficult",                             "mod",
    "Slightly Difficult",                    "mod",
    "Easy",                                  "mil",
    "Very Easy",                             "mil",
    
    "Every Day",                             "sev",
    "Very Often (nearly every day)",         "sev",
    "Fairly Often (2 to 3 times a week)",    "mod",
    "A Few Times (once a week)",             "mod",
    "Once or Twice",                         "mil",
    "None of the Time",                      "mil",
    
    "Very Unhappy",                          "sev",
    "Unhappy",                               "sev",
    "Neither Happy or Unhappy",              "mod",
    "Happy",                                 "mil",
    "Very Happy",                            "mil"
  )
  
  sevdat$sev <- factor(sevdat$sev, levels = c("sev", "mod", "mil"))
  
  # Build labels based on existence of "other" goal -----
  tdat <-
    goal |>
    mutate(Event_Date = as.Date(Event_Date)) |>
    filter(Event_Date == params$Event_Date) |>
    select(Section, Item, Difficulty, Importance) |>
    arrange(Section, Importance) 
    
  
  if(any(goal$Section == "O")){
  seclev <- c("Activities of Daily Living", "Gait Function and Mobility", 
              "Pain, Discomfort, and Fatigue", "Physical Activities, Sports, and Rec.", 
              "Gait Pattern and Appearance",  "Braces and Mobility Aids", 
              "Body Image and Self-Esteem", "Other")
  } else{
    seclev <- c("Activities of Daily Living", "Gait Function and Mobility", 
                "Pain, Discomfort, and Fatigue", "Physical Activities, Sports, and Rec.", 
                "Gait Pattern and Appearance",  "Braces and Mobility Aids", 
                "Body Image and Self-Esteem")
  }
  

  # Check for existing data -----
  if(nrow(tdat) == 0) return(NULL)
  
  tdat <- 
    tdat |>
    mutate(Section = factor(Section, labels = seclev)) |>
    filter(Importance == "Very important") |> 
    left_join(sevdat)
  
  # Check for existing data -----
  if(nrow(tdat) == 0) return(NULL)
  
  t <- 
    tdat |>
    select(-Importance) |> 
    gt(groupname_col = "Section") |>
    tab_header("Very Important GOALs") |> 
    cols_hide(sev) |> 
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_title()
    ) |> 
    tab_style(
      style = cell_text(weight = 600),
      locations = cells_column_labels()
    ) |> 
    tab_style(
      style = cell_text(style = "italic", align = "left", weight = 500),
      locations = cells_row_groups()
    ) |> 
    tab_style(
      style = cell_text(indent = px(40)),
      locations = cells_body(columns = c(Item))
    ) |> 
    tab_style(
      style = cell_text(size = px(14)),
      locations = cells_body()
    ) |> 
    tab_options(
      column_labels.hidden = TRUE
    ) |> 
    # cols_width(
    #   Item ~ px(400),
    #   Difficulty ~ px(300)
    # ) |> 
    data_color(
      columns = sev,
      target_columns = everything(),
      method = "factor",
      domain = domain,
      palette = rev(pal),
      na_color = "white"
    ) 
  
}
