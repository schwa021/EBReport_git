build_checklist <- function(surglist, out_all){
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  # Organize data for tabulating -----
  build_tdat <- function(propside, surgery_, side_){
    var_ <- get_outcome_vars(surgery_)
    Surgery_ <- vlabs[surgery_]
    temp <- propside %>% select(Surgery, side = SIDE, q50)
    
    tdat <- 
      out_all %>% 
      filter(surg == "Effect") %>% 
      mutate(Surgery = vlabs[surgname]) %>% 
      left_join(temp) %>% 
      arrange(side) %>% 
      select(Surgery, var, side, prop = q50, outcome = tau_gt_thresh) %>% 
      filter(Surgery == Surgery_, var %in% var_) %>% 
      mutate(var = factor(var, levels = var_)) %>% 
      arrange(side, var) %>% 
      mutate(
        var = vlabs[as.character(var)],
        side = ifelse(side=="L", "Left", "Right"),
        prop = prop/100
      ) %>% 
      filter(side == side_) %>% 
      select(-side)
    
    # Pivot and rename Struct, Kinem, GDI, FAQT -----
    tdat <- tdat %>% pivot_wider(names_from = var, values_from = outcome)
    names(tdat)[2:6] <- c("prop", "Struct", "Kinem", "GDI", "FAQT")
    names(tdat)[2:6] <- glue("{side_}_{names(tdat)[2:6]}")
    return(tdat)
  }
  
  tempL <- surglist %>% map(\(x) build_tdat(propside=propL, surgery_=x, side_="Left")) %>% list_rbind()
  tempR <- surglist %>% map(\(x) build_tdat(propside=propR, surgery_=x, side_="Right")) %>% list_rbind()
  temp <- left_join(tempL, tempR) %>% mutate(blank="") %>% relocate(blank, .after=Left_FAQT)
  
  # Function to rename columns in final table -----
  rename_cols <- function(names){
    old_names <- names(temp)
    new_names <- str_remove(old_names, "^(Left_|Right_)")
    new_names <- case_when(
      new_names == "Surgery" ~ "Surgery",
      new_names == "blank" ~ "",
      new_names == "Struct" ~ "Structure",
      new_names == "Kinem" ~ "Focal Kinematic",
      new_names == "GDI" ~ "Overall Kinematic",
      new_names == "FAQT" ~ "Mobility",
      new_names == "prop" ~ "Standard of Practice"
    )
    names(new_names) <- old_names
    return(new_names)
  }
  
  # Build table -----
  res <- 
    temp %>% 
    gt() %>%
    data_color(
      columns = matches("prop|Struct|Kinem|GDI|FAQT"),
      method = "bin",
      na_color = "grey90",
      domain = c(0, 1),
      bins = 5,
      palette = pal
    ) %>% 
    cols_width(
      Surgery ~ "180px",
      blank ~ "15px",
      everything() ~ "75px"
    ) %>% 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_column_labels(columns = matches("prop|Struct|Kinem|GDI|FAQT")),
        cells_body(columns = matches("prop|Struct|Kinem|GDI|FAQT"))
      )
    ) %>% 
    fmt(
      columns = matches("prop|Struct|Kinem|GDI|FAQT"),
      fns = function(x) { 
        case_when(
          x < .2 ~ "<b>X</b>",
          x < .4 ~ "X",
          x < .6 ~ "<span style='color: #b3b3b3;'>~</span>",
          x < .8 ~ "\u2713",
          x < 1 ~ "<b>\u2713</b>"
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
    cols_label(!!!rename_cols(temp)) %>% 
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
  
  return(res)
}
