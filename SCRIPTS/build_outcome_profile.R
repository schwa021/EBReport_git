build_outcome_profile <- function(out_all){
  
  # Color palette -----
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  # Prob > thresh for one surgery -----
  p_one_surg <- function(s){
    vv <- c("Structure", "Kinematics", "GDI", "FAQT")
    names(vv) <- fct_inorder(get_outcome_vars(s)) %>% as.character()
    
    res <- 
      out_all %>% 
      filter(
        surgname == s,
        surg == "Treated",
        var %in% names(vv)
      ) %>% 
      select(surgname, side, var, tau_gt_thresh) %>% 
      pivot_wider(names_from = var, values_from = tau_gt_thresh)
    
    names(res)[3:6] <- vv[names(res)[3:6]]
    res <- relocate(res, Structure, Kinematics, GDI, FAQT, .after = side)
    return(res)
  }
  
  lab_one_surg <- function(s){
    vv <- c("Structure", "Kinematics", "GDI", "FAQT")
    names(vv) <- fct_inorder(get_outcome_vars(s)) %>% as.character()
    
    res <- 
      out_all %>% 
      filter(
        surgname == s,
        surg == "Treated",
        var %in% names(vv)
      ) %>% 
      select(surgname, side, var, tau_gt_label) %>% 
      pivot_wider(names_from = var, values_from = tau_gt_label)
    
    names(res)[3:6] <- vv[names(res)[3:6]]
    res <- relocate(res, Structure, Kinematics, GDI, FAQT, .after = side)
    return(res)
  }
  
  # Build for all surgeries -----
  temp <- surglist %>% map(\(x) p_one_surg(x)) %>% list_rbind()
  templab <- 
    surglist %>% 
    map(\(x) lab_one_surg(x)) %>% 
    list_rbind() %>% 
    rename_with(
      .cols = c(Structure, Kinematics, GDI, FAQT),
      ~ glue("lab_{.}")
    )
  temp <- left_join(temp, templab)
  
  # Build table for one side -----
  build_side <- function(temp, sd){
    sdstr <- ifelse(sd == "L", "Left", "Right")
    
    res <- 
      temp %>% 
      filter(side == sd) %>% 
      select(-side) %>% 
      mutate(
        across(
          c(Structure, Kinematics, GDI, FAQT),
          ~ cut(
            ., include.lowest = TRUE, right = FALSE,
            breaks = seq(0, 1, .2),
            labels = c("Unlikely", "Somewhat Unlikely", "Neither", "Somewhat Likely", "Likely")
          )
        )
      ) %>% 
      mutate(surgname = str_replace_all(surgname, "_", " ")) %>% 
      
      gt() %>% 
      cols_label(
        lab_Structure = "Structure",
        lab_Kinematics = "Kinematics",
        lab_GDI = "GDI",
        lab_FAQT = "FAQT"
      ) %>% 
      cols_hide(
        columns = c(Structure, Kinematics, GDI, FAQT) 
      ) %>% 
      cols_label(
        surgname = "Surgery"
      ) %>% 
      tab_header(
        title = md(glue("**{sdstr} Treatment Effect** (\u0394~Treated~ - \u0394~Control~)"))
      ) %>% 
      tab_style( 
        style = cell_text(align = "left"),
        locations = cells_title()
      ) %>% 
      tab_style(
        style = cell_text(weight = 625, align = "center"),
        locations = cells_column_labels(columns = -surgname)
      ) %>% 
      tab_style(
        style = cell_text(weight = 625, align = "left"),
        locations = cells_column_labels(columns = surgname)
      ) %>% 
      tab_style(
        style = cell_text(style = "italic"),
        locations = list(cells_body(columns = surgname))
      ) %>% 
      tab_style(
        style = cell_text(size = "x-small"),
        locations = list(
          cells_source_notes(),
          cells_body(columns = c(lab_Structure, lab_Kinematics, lab_GDI, lab_FAQT))
        )
      ) %>% 
      tab_source_note(
        source_note = md("**Chance of exceeding clinically meaningful threshold <br> 
                         <span style='color:#983A81;'>Unlikely [0%-20%)</span>, 
                         <span style='color:#DBC1D2;'>Somewhat Unlikely [20%-40%)</span>,
                         <span style='color:#808080;'>Neither [40%-60%)</span>, 
                         <span style='color:#BCCDB7;'>Somewhat Likely [60%-80%)</span>,
                         <span style='color:#2B6C00;'>Likely [80%-100%)</span>**")
      ) %>%
      cols_width(
        surgname ~ px(275),
        everything() ~ px(150)
      ) %>% 
      data_color(
        columns = Structure,
        target_columns = lab_Structure,
        method = "factor",
        levels = c("Likely", "Somewhat Likely", "Neither", "Somewhat Unlikely", "Unlikely"),
        palette = rev(pal),
        alpha = 1,
        domain = c(0, 100),
        autocolor_text = TRUE
      ) %>% 
      data_color(
        columns = Kinematics,
        target_columns = lab_Kinematics,
        method = "factor",
        levels = c("Likely", "Somewhat Likely", "Neither", "Somewhat Unlikely", "Unlikely"),
        palette = rev(pal),
        alpha = 1,
        domain = c(0, 100),
        autocolor_text = TRUE
      ) %>% 
      data_color(
        columns = GDI,
        target_columns = lab_GDI,
        method = "factor",
        levels = c("Likely", "Somewhat Likely", "Neither", "Somewhat Unlikely", "Unlikely"),
        palette = rev(pal),
        alpha = 1,
        domain = c(0, 100),
        autocolor_text = TRUE
      ) %>% 
      data_color(
        columns = FAQT,
        target_columns = lab_FAQT,
        method = "factor",
        levels = c("Likely", "Somewhat Likely", "Neither", "Somewhat Unlikely", "Unlikely"),
        palette = rev(pal),
        alpha = 1,
        domain = c(0, 100),
        autocolor_text = TRUE
      )
    
    return(res)
  }
  
  outprofL <- build_side(temp, "L")
  outprofR <- build_side(temp, "R")
  
  return(list(outprofL = outprofL, outprofR = outprofR))
  
}
