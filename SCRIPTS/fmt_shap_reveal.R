fmt_shap <- function(tall, ptprops, s, side, brdcol){
  ss <- ifelse(side == "L", "Left", "Right")
  tit <- str_replace_all(s, "_", " ")
  # tit <- glue("{ss} - {tit}")
  
  t <- tall[[s]] |> 
    filter(SIDE == side)
  
  p <- ptprops |> 
    filter(
      Surgery == str_replace_all(s, "_", " "), 
      SIDE == side
    )
  
  subtit <- glue("Propensity = {p$q50}% ({p$q5}%, {p$q95}%)<br>*Range derived from estimated measurement error*")
  
  # Input sentence
  source_note_html <- htmltools::HTML("<span style='font-style: italic;'>Indications: <span style='color: #005600; font-weight: bold'>strong</span>, <span style='color: #7CC57D; font-weight: bold'>weak</span>. Counterindications <span style='color: #841859; font-weight: bold'>strong</span>, <span style='color: #F398C4; font-weight: bold'>weak</span></span>")
  
  # Make the table
  tbl <- 
    t %>% 
    select(phix, lab, value) %>% 
    arrange(desc(phix)) %>% 
    gt() %>% 
    tab_header(
      title = tit,
      subtitle = md(subtit)
    ) %>%  
    opt_align_table_header(align = "left") %>% 
    cols_hide(phix) %>% 
    cols_label(
      lab = "Patient Characteristic",
      value = "Value"
    ) %>% 
    # cols_width(
    #   value ~ px(150)
    # ) %>% 
    cols_align(
      align = "center",
      columns = value
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(),
        cells_title(groups = c("title"))
      )
    ) %>% 
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_body(columns = lab)
    ) %>% 
    
    tab_source_note(source_note_html) |>
    
    data_color(
      columns = phix,
      method = "bin",
      target_columns = everything(),
      palette = c("#841859", "#F398C4", "#F9F9F9", "#188B41", "#004616"),
      bins = 5,
      domain = c(-.5, .5),
      autocolor_text = T
    ) |>  
    opt_table_outline(color = brdcol, width = px(4))
}