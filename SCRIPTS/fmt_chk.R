fmt_chk <- function(t, s){
  
  # Make title
  tit <- str_replace_all(s, "_", " ")
  tit <- glue("Matching Quality for {tit}")
  
  t <- t |> 
    group_by(grp) |> 
    arrange(desc(grp))
  
  # Make table -----
  tblchk <- 
    t %>% 
    gt() %>% 
    tab_header(title = tit) |> 
    sub_missing(missing_text = "---") %>% 
    cols_label(
      Limits_L = "Left",
      Limits_R = "Right",
      Characteristic = "Characteristic"
    ) %>% 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_body(columns = c(Limits_L, Limits_R)),
        cells_column_labels(columns = c(Limits_L, Limits_R))
      )
    ) |> 
    tab_style_body(
      style = list(
        cell_fill(color = "#005600", alpha = 1),
        cell_text(color = "#F6F6F6")
      ),
      values = "good",
    ) %>% 
    tab_style_body(
      style = cell_fill(color = "#F6F6F6", alpha = 1),
      values = "fair",
    ) %>% 
    tab_style_body(
      style = list(
        cell_fill(color = "#841859", alpha = 1),
        cell_text(color = "#F6F6F6")
      ),
      values = "poor"
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(cells_column_labels(), cells_row_groups())) %>% 
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_title()
    ) |> 
    tab_style(
      style = cell_text(indent = px(20)),
      locations = cells_body(columns = Characteristic)
    ) |> 
    cols_width(
      "Characteristic" ~ px(370),
      starts_with("Limits") ~ px(120)
    )
}
