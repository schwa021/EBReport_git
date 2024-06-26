ptableLR <- function(datL, datR){
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  profL <- 
    datL %>% 
    select(Surgery, q50) |> 
    mutate(
      breaks = cut(
        q50, c(-999, 20, 40, 60, 80, 999), 
        right = F,
        labels = c("Unlikely", "Somewhat Unlikely", "Neither", "Somewhat Likely", "Likely")
      )
    ) %>% 
    select(Surgery, breaks) |> 
    rename(breaksL = breaks)
  
  profR <- 
    datR %>% 
    select(Surgery, q50) |> 
    mutate(
      breaks = cut(
        q50, c(-999, 20, 40, 60, 80, 999), 
        right = F,
        labels = c("Unlikely", "Somewhat Unlikely", "Neither", "Somewhat Likely", "Likely")
      )
    ) %>% 
    select(Surgery, breaks) |> 
    rename(breaksR = breaks)
  
  prof <- left_join(profL, profR)
  
  
  tbl <- 
    prof |> 
    gt() %>% 
    
    tab_header(
      title = md("**Patient Propensity Profile**")
    ) %>% 
    tab_style( 
      style = cell_text(align = "left"),
      locations = cells_title()
    ) %>% 
    cols_label(
      breaksL = "Left",
      breaksR = "Right"
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
      style = cell_text(size = "small"),
      locations = cells_source_notes()
    ) %>% 
    tab_source_note(source_note = "Unlikely [0%-20%), Somewhat Unlikely [20%-40%), Neither [40%-60%), Somewhat Likely [60%-80%), Likely [80%-100%)") %>%
    cols_width(
      Surgery ~ px(280),
      breaksL ~ px(180),
      breaksR ~ px(180)
    ) %>% 
    data_color(
      columns = c(breaksL, breaksR),
      method = "factor",
      levels = c("Likely", "Somewhat Likely", "Neither", "Somewhat Unlikely", "Unlikely"),
      palette = rev(pal),
      alpha = 1,
      domain = c(0, 100),
      autocolor_text = TRUE
    ) %>% 
    opt_stylize(style = 1, color = "gray", add_row_striping = FALSE) 
  # |>
  #   as_raw_html()
}