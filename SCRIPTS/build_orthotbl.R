build_orthotbl <- function(xpt, meas){
  
  domain <- c(1, 2, 3, 4)
  # Choose palette -----
  clr <- sequential_hcl(n = 4, h = 360, c = c(85, NA, NA), l = c(55, 90), power = 1.8)
  pal <- rev(clr)
  
  # Organize data -----
  if(any(meas == "PATELLA_ALTA")){
    tdat <- 
      xpt |> 
      select(all_of(c("SIDE", {{meas}}))) |> 
      mutate(
        PATELLA_ALTA = case_when(
          PATELLA_ALTA=="N" ~ 0,
          PATELLA_ALTA=="Y" ~ 1,
          TRUE ~ NA
        )
      )
  } else {
    tdat <- 
      xpt |> 
      select(all_of(c("SIDE", {{meas}})))
  }
  
  tdat <- 
    tdat |> 
    pivot_longer(-SIDE) |> 
    pivot_wider(names_from = SIDE, values_from = value) |> 
    rename(Left = L, Right = R) |> 
    mutate(Measure = vlabs[name]) |> 
    relocate(Measure) 
  
  tdat <- 
    tdat |> 
    mutate(
      Level = case_when(
        str_detect(Measure, "Hip Flex|Hip Ext|Hip Abd") ~ "Hip",
        str_detect(Measure, "Trochanteric|Internal Hip|External Hip|Femoral") ~ "Femur",
        str_detect(Measure, "Ankle") ~ "Ankle",
        str_detect(Measure, "Knee Flexion|Knee Extension|Lag|Patella|Popl") ~ "Knee",
        str_detect(Measure, "Bimal|Second Toe") ~ "Tibia"
      )
    )
  
  # Compute Severity -----
  tdat <- orthoseverity(tdat, xpt$age[1], xpt$Sex[1])
  
  
  # Convert Patella Alta back to text -----
  if(any(meas == "PATELLA_ALTA")){
    tdat$Left[tdat$name == "PATELLA_ALTA"] <- ifelse(tdat$Left[tdat$name=="PATELLA_ALTA"]==1, "Yes", "No")
    tdat$Right[tdat$name == "PATELLA_ALTA"] <- ifelse(tdat$Right[tdat$name=="PATELLA_ALTA"]==1, "Yes", "No")
  }
  
  # Generate table -----
  t <- 
    tdat |> 
    gt(
      groupname_col = "Level"
    ) |> 
    
    cols_hide(name) |> 
    sub_missing( ) |> 
    tab_style(
      style = cell_text(style = "italic", indent = px(20), size = "medium"),
      locations = cells_body(columns = Measure)
    ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels()
      )
    ) |> 
    tab_style(
      style = cell_text(color = "white"),
      locations = cells_column_labels(columns = Measure)
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_body(columns = c(Left, Right)),
        cells_column_labels()
      )
    ) |> 
    cols_width(
      Left ~ px(100),
      Right ~ px(100)
    )  |> 
    cols_hide(c(Lsev, Rsev)) |> 
    data_color(
      columns = c(Lsev, Rsev),
      target_columns = c(Left, Right),
      method = "factor",
      domain = domain,
      palette = pal,
      na_color = "white"
    )  
  return(t)
}
