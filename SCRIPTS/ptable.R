ptable <- function(d){
  
  
  prof <- 
    d %>% 
    select(Surgery, q50) |> 
    mutate(breaks = cut(q50, c(-999, 20, 40, 60, 80, 999), right = F, 
                        labels = c("Unlikely", "Somewhat Unlikely", "Neither", "Somewhat Likely", "Likely"))) %>% 
    select(Surgery, breaks, q50) %>% 
    
    gt() %>% 
    
    cols_hide(q50) %>% 
    tab_header(
      title = md("**Patient Propensity Profile**")
    ) %>% 
    tab_style( 
      style = cell_text(align = "left"),
      locations = cells_title()
    ) %>% 
    cols_label(
      breaks = "Historical Standard"
    ) %>% 
    tab_style(
      style = cell_text(weight = 625),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = cell_text(style = "italic"),
      locations = list(cells_body(columns = Surgery), cells_source_notes())
    ) %>% 
    tab_style(
      style = cell_text(size = "x-small"),
      locations = cells_source_notes()
    ) %>% 
    tab_source_note(source_note = "Unlikely [0%-20%), Somewhat Unlikely [20%-40%), Neither [40%-60%), Somewhat Likely [60%-80%), Likely [80%-100%)") %>%
    cols_width(
      Surgery ~ px(250),
      breaks ~ px(150)
    ) %>% 
    data_color(
      columns = q50,
      method = "bin",
      target_columns = everything(),
      # palette = "RdYlGn",
      palette = rev(c("#004616", "#81c07a", "#cbcfc9", "#ffffff", "#ffffff")),
      alpha = .5,
      bins = 5,
      domain = c(0, 100),
      autocolor_text = FALSE
    ) %>% 
    opt_stylize(style = 1, add_row_striping = FALSE)
}