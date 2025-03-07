fmt_shapLR <- function(tall, ptprops, s){

  tit <- str_replace_all(vlabs[s], "_", " ")
  
  t <- tall[[s]]
  
  p <- ptprops |> 
    filter(
      Surgery == str_replace_all(vlabs[s], "_", " ")
    )
  
  # Get fill color for propensity value/range row -----
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  pfill <- function(x){
    y <- case_when(
      x < 20 ~ pal[1],
      x < 40 ~ pal[2],
      x < 60 ~ pal[3],
      x < 80 ~ pal[4],
      TRUE ~ pal[5]
    )
  }
  pcolor <- function(x){
    y <- case_when(
      x < 20 ~ "white",
      x < 40 ~ "grey20",
      x < 60 ~ "grey20",
      x < 80 ~ "grey20",
      TRUE ~ "white"
    )   
  }
  pfillL <- pfill(p$q50[p$SIDE == "L"])
  pfillR <- pfill(p$q50[p$SIDE == "R"])
  pcolorL <- pcolor(p$q50[p$SIDE == "L"])
  pcolorR <- pcolor(p$q50[p$SIDE == "R"])
  
  # Input sentence
  source_note_html <- htmltools::HTML("<span style='font-style: italic;'>Indications: <span style='color: #005600; font-weight: bold'>strong</span>, <span style='color: #ABDFAC; font-weight: bold'>weak</span>. Counterindications <span style='color: #841859; font-weight: bold'>strong</span>, <span style='color: #FFBFDE; font-weight: bold'>weak</span></span>")
  
  tbl <- 
    t |> 
    mutate(
      phix = cut(phix, c(-1, -.2, -.05, -.01, .01, .05, .2, 1), labels = c(1, 2, 3, 4, 5, 6, 7))
    ) |> 
    pivot_wider(
      names_from = SIDE, 
      values_from = c(phix, imp, lab, value), 
      values_fn = list
    ) |> 
    mutate(blank = " ") |> 
    unnest(cols = everything()) |> 
    select(-Surgery, -imp_L, -imp_R) |> 
    relocate(phix_L, lab_L, value_L, blank, phix_R, lab_R, value_R)
  
  tLR <- 
    tbl |> 
    gt() |> 
    tab_header(
      title = tit
    ) %>%  
    opt_align_table_header(align = "left") %>% 
    cols_hide(starts_with("phi")) %>% 
    cols_label(
      lab_L = "Characteristic",
      lab_R = "Characteristic",
      value_L = "Value",
      value_R = "Value",
    ) %>% 
    tab_style(
      style = cell_text(color = "white"),
      locations = cells_column_labels(columns = "blank")
    ) |> 
    tab_spanner(
      label =  glue("Left Propensity = {p$q50[1]}% ({p$q5[1]}%, {p$q95[1]}%)"),
      columns = c(lab_L, value_L), 
      id = "L"
    ) |> 
    tab_spanner(
      label = glue("Right Propensity = {p$q50[2]}% ({p$q5[2]}%, {p$q95[2]}%)"),
      columns = c(lab_R, value_R),
      id = "R"
    ) |> 
    tab_style(
      style = list(cell_fill(color = pfillL), cell_text(color = pcolorL)),
      locations = cells_column_spanners(spanners = "L")
    ) |> 
    tab_style(
      style = list(cell_fill(color = pfillR), cell_text(color = pcolorR)),
      locations = cells_column_spanners(spanners = "R")
    ) |> 
    cols_width(
      starts_with("value") ~ px(145),
      starts_with("lab") ~ px(230),
      "blank" ~ px(20)
    ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(),
        cells_column_spanners(),
        cells_title(groups = c("title"))
      )
    ) |> 
    tab_style(
      style = cell_text(style = "italic"),
      locations = list(
        cells_column_spanners()
      )
    ) |> 
    # tab_style(
    #   style = cell_text(style = "italic"),
    #   locations = cells_body(columns = starts_with("lab"))
    # ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_body(columns = starts_with("val")),
        cells_column_labels(starts_with("val"))
      )
    ) |> 
    
    # tab_source_note(source_note_html) |>
    data_color(
      columns = phix_L,
      method = "factor",
      target_columns = c(lab_L, value_L),
      palette = diverging_hcl(n = 7, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8),
      domain = c(1, 2, 3, 4, 5, 6, 7),
      autocolor_text = T
    ) |> 
    data_color(
      columns = phix_R,
      method = "factor",
      target_columns = c(lab_R, value_R),
      palette = diverging_hcl(n = 7, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8),
      domain = c(1, 2, 3, 4, 5, 6, 7),
      autocolor_text = T
    ) 
}

