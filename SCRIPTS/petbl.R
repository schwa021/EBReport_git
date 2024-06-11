petbl <- function(tdat, tit){
  
  t <- 
    tdat |> 
    gt(
      groupname_col = "Type"
    ) |> 
    cols_hide(name) |> 
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
    )
  
  return(t)
}