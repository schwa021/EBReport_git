# ---- Format outcome table ----
fmt_checklist <- function(xx) {
  # Function to rename columns in final table -----
  rename_cols <- function(xx){
    old_names <- names(xx)
    new_names <- str_remove(old_names, "^(Left_|Right_)")
    new_names <- case_when(
      new_names == "surgery" ~ "Surgery",
      new_names == "blank" ~ "",
      new_names == "Structure" ~ "Structure",
      new_names == "Kinematic" ~ "Focal Kinematic",
      new_names == "GDI" ~ "Overall Kinematic",
      new_names == "FAQT" ~ "Mobility",
      new_names == "prop" ~ "Standard of Practice"
    )
    names(new_names) <- old_names
    return(new_names)
  } 
  # Choose pallette for bad/good ----
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  # Create summary table ----
  res <- 
    xx %>% 
    relocate(Left_prop, .after = surgery) %>%
    relocate(Right_prop, .after = blank) %>%
    gt() %>% 
    data_color(
      columns = matches("prop|Struct|Kinem|GDI|FAQT"),
      method = "bin",
      na_color = "grey90",
      domain = c(0, 100),
      bins = 5,
      palette = pal
    )  %>% 
    cols_width(
      surgery ~ "150px",
      blank ~ "15px",
      everything() ~ "80px"
    ) %>% 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_column_labels(columns = matches("prop|Struct|Kinem|GDI|FAQT|Left|Right")),
        cells_body(columns = matches("prop|Struct|Kinem|GDI|FAQT"))
      )
    ) %>% 
    fmt(
      columns = matches("prop|Struct|Kinem|GDI|FAQT"),
      fns = function(x) { 
        case_when(
          x < 20 ~ "<b>X</b>",
          x < 40 ~ "X",
          x < 60 ~ "<span style='color: #b3b3b3;'>~</span>",
          x < 80 ~ "\u2713",
          x < 999 ~ "<b>\u2713</b>"
        )
      }
    ) %>%
    tab_style(
      style = cell_borders(
        color = "#b3b3b3",
        weight = px(1)
      ),
      locations = cells_body(
        columns = matches("prop|Struct|Kinem|GDI|FAQT")
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>% 
    tab_source_note(
      source_note = md("\u2713 = Criterion met, X = Criterion not met, ~ = Uncertain<br>
                     **\u2713/X** = high confidence, \u2713/X = moderate confidence<br>
                       Overall Kinematic = Gait Deviation Index (GDI)<br> 
                       Mobilitiy = Functional Assessment Questionnaire Transform (FAQT)")
    ) %>%
    tab_options(
      column_labels.border.top.color = "transparent"
    ) %>% 
    cols_label(!!!rename_cols(xx)) %>% 
    tab_spanner(
      label = "Outcome",
      columns = c(3:6, 9:12),
      gather = FALSE
    ) %>%
    tab_spanner(
      label = "Treatment",
      columns = c(2, 8),
      gather = FALSE
    ) %>%
    tab_spanner(
      label = "Left",
      columns = c(2:6)
    ) %>% 
    tab_spanner(
      label = "Right",
      columns = c(8:12)
    ) %>% 
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_column_spanners(spanners = c("Left", "Right"))
    ) %>% 
    tab_options(
      table.font.size = px(12)
    ) %>% 
    tab_style(
      style = cell_borders(
        side = c("left", "right"),
        color = "black",
        weight = px(2)
      ),
      locations = cells_body(columns = matches("prop"))
    ) %>% 
    sub_missing(
      columns = everything(),
      missing_text = "Missing",
    )
  
  # Return formatted table ----
  return(res)
}
