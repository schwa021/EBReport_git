# ---- Organize outcomes Left and Right with blank in middle ----
organize_outcome <- function(xx, s, sd, st){
  xx <- 
    xx %>% 
    filter(
      surgery == vlabs[s],
      side == sd,
      status == st
    ) %>% 
    select(surgery, var, good_prob) %>% 
    mutate(var = c("Structure", "Kinematic", "GDI", "FAQT")) %>% 
    mutate(var = glue("{sd}_{var}")) %>% 
    pivot_wider(names_from = var, values_from = good_prob)
  return(xx)
}

