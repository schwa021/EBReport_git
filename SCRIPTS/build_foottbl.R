build_foottbl <- function(xpt, meas){
  
  # Get levels of severity based on type of measure -----
  type <- case_when(
    str_detect(meas[1], "WB") ~ "wb",
    str_detect(meas[1], "NWB") ~ "nwb"
  )
  domain <- c(1, 2, 3, 4, 5)
  clr <- sequential_hcl(n = 5, h = 360, c = c(85, NA, NA), l = c(55, 90), power = 1.8)
  pal <- rev(clr)
  pal <- c(pal, "#f7e0a1")
  
  # Build table -----
  tdat <- 
    xpt |> 
    select(all_of(c("SIDE", {{meas}}))) |> 
    pivot_longer(-SIDE) |> 
    pivot_wider(names_from = SIDE, values_from = value)
  
  if(any(tdat$name == "NWB_PF1")){
    temp <- tdat[tdat$name == "NWB_PF1",]
    temp$name <- "NWB_PF1_SEV"
    temp$L <- ifelse(temp$L== "TYP", "NONE", temp$L)
    temp$R <- ifelse(temp$R == "TYP", "NONE", temp$R)
    tdat <- bind_rows(tdat, temp)
  }
  
  tval <- 
    tdat |> 
    filter(!str_detect(name, "SEV")) 

  tsev <- 
    tdat |> 
    filter(str_detect(name, "SEV")) |> 
    rename(Lsev = L, Rsev = R) |> 
    mutate(name = str_remove_all(name, "_SEV"))
  
  tdat <- 
    left_join(tval, tsev) |> 
    mutate(
      across(
        matches("sev"),
        ~ ifelse(is.na(.), "UNK", as.character(.))
      )
    )|> 
    mutate(
      across(
        matches("sev"),
        ~ as.numeric(factor(., levels = c("NONE", "MIL", "MOD", "SEV", "UNK")))
      )
    ) |> 
    rename(Left = L, Right = R) |> 
    mutate(Measure = vlabs[name]) |> 
    relocate(Measure) 
  
  for (kk in 1:nrow(tdat)) {
    if(tdat$Left[kk] == "TYP") tdat$Lsev[kk] <- 1
    if(tdat$Right[kk] == "TYP") tdat$Rsev[kk] <- 1
  }
  
  t <- 
    tdat |> 
    gt() |> 
    cols_hide(c(name, Lsev, Rsev)) |> 
    sub_missing() |> 
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
    data_color(
      columns = c(Lsev, Rsev),
      target_columns = c(Left, Right),
      # method = "factor",
      domain = domain,
      palette = pal,
      na_color = "white"
    )
  
  return(t)
}
  