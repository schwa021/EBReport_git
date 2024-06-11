tbl_pt_vs_match <- function(d, v, stat){
  
  clr <- ifelse(stat == "Pre", myred, myblue)

  tbl <- 
    d %>% 
    bind_rows(xpt) %>% 
    mutate(target = ifelse(target == 0, "Match", "Patient")) %>% 
    select(all_of(v), target) %>% 
    tbl_summary(
      by = target,
      type = where(is.numeric) ~ "continuous",
      missing = "no"
    ) %>% 
    italicize_levels() %>% 
    bold_labels() %>%
    as_gt() %>% 
    opt_stylize(add_row_striping = F) %>% 
    tab_style(
      style = cell_text(color = clr),
      locations = list(
        cells_body(columns = stat_2),
        cells_column_labels(columns = stat_2)
      )
    ) %>% 
    tab_style(
      style = cell_text(color = clr),
      locations = list(
        cells_body(columns = stat_2),
        cells_column_labels(columns = stat_2)
      )
    ) %>% 
    tab_style(
      style = cell_text(color = "white"),
      locations = list(cells_column_labels(columns = label))
    )
  
  return(tbl)
}
