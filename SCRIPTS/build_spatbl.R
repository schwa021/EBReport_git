build_spatbl <- function(xpt, meas){
  
  domain <- c(0, 1, 2, 3, 4)
  # Choose palette -----
  clr <- sequential_hcl(n = 5, h = 360, c = c(85, NA, NA), l = c(55, 90), power = 1.8)
  pal <- rev(clr)

  # Build table -----
  sev <- function(x){
    x <- 
      case_when(
        x %in% (c(0, "Absent")) ~ 0,
        x %in% (c(1, "Unsustained")) ~ 1,
        x %in% (2) ~ 2,
        x %in% (c(3, 4, "Sustained")) ~ 3,
        x %in% ("Missing") ~ NA
      )  
    x <- factor(x, levels = c(0, 1, 2, 3, 4))
  }
  
  tdat <- 
    xpt |>
    select(all_of(c("SIDE", {{meas}}))) |> 
    pivot_longer(-SIDE) |> 
    pivot_wider(names_from = SIDE, values_from = value) |> 
    rename(Left = L, Right = R) |> 
    mutate(Measure = vlabs[name]) |> 
    relocate(Measure) |> 
    mutate(
      Lsev = sev(Left),
      Rsev = sev(Right)
    ) |> 
    mutate(
      across(
        matches("^Left|Right"),
        ~ as.character(.)
      )
    ) |> 
    mutate(
      across(
        matches("^Left|^Right"),
        ~ ifelse(. == "Missing", NA, .)
      )
    )
  
  t <- 
    tdat |> 
    gt() |> 
    cols_hide(name) |> 
    sub_missing() |> 
    cols_hide(c(Lsev, Rsev)) |> 
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
      method = "factor",
      levels = domain,
      palette = pal,
      na_color = "white"
    ) |> 
    tab_style_body(
      style = cell_text(color = "transparent"),
      pattern = "Missing"
    ) 
  
  return(t)
}
