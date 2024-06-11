build_neurotbl <- function(xpt, meas){
  
  # Get levels of severity based on type of measure -----
  type <- case_when(
    str_detect(meas[1], "SPAS") ~ "spa",
    str_detect(meas[1], "SEL") ~ "sel",
    str_detect(meas[1], "STR") ~ "str"
  )
  
  if (type == "str") {
    domain <- c(1, 1, 2, 3, 4, 5)
    pal <-  sequential_hcl(length(domain), "Reds 3")
  } else if (type == "sel") {
    domain <- c(0, 1, 2)
    pal <-  sequential_hcl(length(domain), "Reds 3")
  } else {
    domain <- c(0, 1, 2, 3, 4)
    pal <-  rev(c("#69000C", "#69000C", "#EE253A", "#FFA6AA", "#F6F6F6"))
  }
  
  # Build table -----
  t <- 
    xpt |> 
    select(all_of(c("SIDE", {{meas}}))) |> 
    pivot_longer(-SIDE) |> 
    pivot_wider(names_from = SIDE, values_from = value) |> 
    rename(Left = L, Right = R) |> 
    mutate(Measure = vlabs[name]) |> 
    relocate(Measure) |> 
    gt() |> 
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
    )  |> 
    data_color(
      columns = c(Left, Right),
      method = "factor",
      domain = domain,
      palette = pal
    )
  
  return(t)
}
