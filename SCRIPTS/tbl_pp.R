tbl_pp <- function(d, v){
  
  # Compute changes -----
  for (vtarget in v) {
    if(is.factor(d[[glue("{vtarget}")]])){
      d[[glue("{vtarget}Change")]] <- NA
    } else {
      d[[glue("{vtarget}Change")]] <- d[[glue("{vtarget}Post")]] - d[[glue("{vtarget}")]]
    }
  }
  
  # Get post and Change names -----
  vppd <- c(v, glue("{v}Post"), glue("{v}Change"))
  
  
  # Find factors -----
  vx <- 
    d %>% 
    select({{vppd}}) %>% 
    select(where(is.factor)) %>% 
    names()
  
  vxpre <- str_subset(vx, "Post", negate = T)
  
  
  # Organize data -----
  dtbl <- 
    d %>% 
    select(Exam_ID, SIDE, {{vppd}}) %>% 
    mutate(
      across(
        {{vx}},
        ~ as.numeric(.)
      )
    ) %>%
    pivot_longer(-c(Exam_ID, SIDE)) %>% 
    mutate(
      status = case_when(
        str_detect(name, "Post") ~ "Post",
        str_detect(name, "Change") ~ "Change",
        TRUE ~ "Pre"
      ),
      status = factor(status, levels = c("Pre", "Post", "Change"))
    ) %>% 
    mutate(
      name = str_replace_all(name, "Post|Change", "")
    ) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    select(-Exam_ID, -SIDE)
  
  
  # Convert factors back to original - can't figure out dplyr soln -----
  if(length(vxpre)> 0){
    for (vv in vxpre) {
      dtbl[[vv]] <- levels(d[[vv]])[dtbl[[vv]]]
      dtbl[[vv]] <- fct_na_value_to_level(dtbl[[vv]], "Missing")
    }
  }
  
  
  # Make table -----
  tbl <- 
    dtbl %>% 
    tbl_summary(
      by = status,
      type = where(is.numeric) ~ "continuous",
      statistic = all_categorical() ~"{p}%",
      digits = everything() ~ 0
    ) %>% 
    italicize_levels() %>% 
    bold_labels()
  
  tblgt <- 
    tbl %>% 
    as_gt() %>%
    opt_stylize(add_row_striping = F) %>%
    tab_style(
      style = list(cell_text(color = "transparent")),
      locations = cells_body(
        columns = stat_3,
        rows = str_detect(stat_3, "%") | label %in% c("Unknown", "Missing")
      )
    ) %>% 
    tab_style(
      style = list(cell_fill(color = "grey95")),
      locations = cells_body(
        columns = stat_3,
        rows = everything()
      )
    )
  
  return(tblgt)
  
}