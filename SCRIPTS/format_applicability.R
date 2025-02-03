format_applicability <- function(res, surglist, xpt){
  
  Lq <- lapply(res, "[[", 1)
  # Lp <- lapply(res, "[[", 2)
  
  names(Lq) <- c(glue("Diagnosis = {xpt[1,]$dx}"), glue("mod_{surglist}"))
  # names(Lp) <- c("Diagnosis", glue("mod_{surglist}"))
  
  prox <- 
    tibble(
      name = names(Lq),
      Left_Q = sapply(Lq, "[", 1),
      Right_Q = sapply(Lq, "[", 2)
    ) %>% 
    mutate(
      Left = cut(Left_Q, breaks = c(-Inf, 95, 99, Inf), labels = c("High", "Medium", "Low")),
      Right = cut(Right_Q, breaks = c(-Inf, 95, 99, Inf), labels = c("High", "Medium", "Low"))
    ) %>% 
    mutate(
      name = str_replace(name, "mod_", ""),
      name = str_replace_all(name, "_", " ")
    ) %>% 
    select(-Left_Q, -Right_Q)
  
  # Make affected text -----
  affected <- ifelse(xpt$affected==TRUE, "Affected", "Unaffected")
  
  applicability <- 
    prox %>% 
    gt() %>%
    cols_label(
      name = "",
      Left = glue("**Left**<br>*{affected[1]}*"),
      Right = glue("**Right**<br>*{affected[2]}*"),
      .fn = md
    ) %>% 
    tab_header("Model Applicability") %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = list(cells_body(columns = "name"), cells_column_labels(columns = "name"))
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "left")
      ),
      locations = cells_title(groups = "title")
    ) %>%
    cols_width(
      "name" ~ 300,
      everything() ~ 150,
    ) %>%
    data_color(
      columns = c(2,3),
      method = "factor",
      palette = c("#008450", "#EFB700", "#B81D13"),
    ) %>%
    tab_source_note(
      source_note = md("The table shows how similar (**High**) or dissimilar (**Low**) the patient's data is to training data used to build the propensity and outcome models. Model accuracy **may** be lower for patients who are dissimilar to the training data. Results for these patients should be interpreted with extra caution. Note that diagnosis is evaluated independently.")
    ) %>% 
    tab_style(
      style = cell_text(size = px(12), style="italic", color="grey60"),
      locations = cells_source_notes()
    )
  
  return(applicability)
}
