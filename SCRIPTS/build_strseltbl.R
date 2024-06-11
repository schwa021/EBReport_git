build_strseltbl <- function(xpt){
  
  # Choose measures -----
  meas <- str_subset(names(xpt), ("_SEL$|_STR$"))
  
  # Choose palette -----
  clr <- sequential_hcl(n = 4, h = 360, c = c(85, NA, NA), l = c(55, 90), power = 1.8)
  # "#D65979" "#E4A2AE" "#E4D0D3" "#E2E2E2"
  domainstr <- factor(c(0, 1, 2, 3, 4, 5))
  palstr <- c(clr[c(1, 1, 1, 2, 3, 4)])
  # "#D65979" "#D65979" "#D65979" "#E4A2AE" "#E4D0D3" "#E2E2E2"
  domainsel <- factor(c(0, 1, 2))
  palsel <- clr[c(1, 2, 4)]

  
  # Build data -----
  tdat <- 
    xpt |> 
    select(all_of(c("SIDE", meas))) |> 
    pivot_longer(-SIDE) |> 
    pivot_wider(names_from = SIDE, values_from = value) |> 
    mutate(name = vlabs[name]) |> 
    mutate(
      pref = str_match(name, "^(.+) (Motor Control|Strength)$")[,2],
      suff = str_match(name, "^(.+) (Motor Control|Strength)$")[,3],
      name = pref,
      pref = NULL,
      suff = str_replace_all(suff, " ", "_")
    ) |> 
    rename(Measure = name) |> 
    pivot_wider(
      names_from = suff,
      values_from = c(L, R)
    ) |> 
    relocate(R_Motor_Control, .after = L_Motor_Control) |> 
    mutate(blank = " ") |> 
    relocate(blank, .before = L_Strength) |> 
    mutate(
      Level = case_when(
        str_detect(Measure, "Abdom|Back") ~ "Core",
        str_detect(Measure, "Tibial|Plantarfl|Peroneu|Hallu") ~ "Ankle/Foot",
        str_detect(Measure, "Hip") ~ "Hip",
        str_detect(Measure, "Knee") ~ "Knee"
      )
    ) |> 
    mutate(
      Level = factor(Level, levels = c("Core", "Hip", "Knee", "Ankle/Foot"))
    ) |> 
    mutate(
      across(
        matches("Strength$"),
        ~ factor(., levels = c(0, 1, 2, 3, 4, 5), exclude = "Missing")
      )
    ) |> 
    mutate(
      across(
        matches("Control$"),
        ~ factor(., levels = c(0, 1, 2), exclude = "Missing")
      )
    )
    
  # Build table -----
  t <- 
    tdat |> 
    gt(
      groupname_col = "Level"
    ) |> 
    row_group_order(groups = levels(tdat$Level)) |> 
    sub_missing() |> 
    cols_label(
      L_Motor_Control = "Left",
      L_Strength = "Left",
      R_Motor_Control = "Right",
      R_Strength = "Right"
    ) |> 
    tab_spanner(
      label = "Motor Control",
      columns = c(L_Motor_Control, R_Motor_Control)
    ) |> 
    tab_spanner(
      label = "Strength",
      columns = c(L_Strength, R_Strength)
    ) |> 
    tab_style(
      style = cell_text(style = "italic", indent = px(20), size = "medium"),
      locations = cells_body(columns = Measure)
    ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(),
        cells_column_spanners()
      )
    ) |> 
    tab_style(
      style = cell_text(color = "white"),
      locations = cells_column_labels(columns = c(Measure, blank))
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_body(columns = -Measure),
        cells_column_labels()
      )
    ) |> 
    cols_width(
      starts_with("L") ~ px(100),
      starts_with("R") ~ px(100)
    )  |> 
    data_color(
      columns = c(L_Motor_Control, R_Motor_Control),
      method = "factor",
      domain = domainsel,
      palette = palsel,
      na_color = "white"
    )  |> 
    data_color(
      columns = c(L_Strength, R_Strength),
      method = "factor",
      domain = domainstr,
      palette = palstr,
      na_color = "white"
    ) |> 
    tab_style_body(
      style = cell_text(color = "transparent"),
      pattern = "Missing"
    ) 
  
  return(t)
}

