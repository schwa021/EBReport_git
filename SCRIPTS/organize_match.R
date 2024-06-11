organize_match <- function(dd_match_x, measured_x, stat){
  
  dd_match_x_avg <- 
    dd_match_x %>% 
    group_by(t, name) %>% 
    reframe(
      lwr = quantile(value, probs = .05, na.rm = T),
      upr = quantile(value, probs = .95, na.rm = T),
      value = quantile(value, probs = .5, , na.rm = T)
    ) %>% 
    mutate(
      type = "Matched",
      status = {{stat}}
    ) %>% 
    filter(
      !(name %in% c("Ank.Ang.Cor", "Ank.Ang.Trn", "Foo.Ang.Cor"))
    ) %>% 
    mutate(name = fct_drop(name))
  
  # Organize matching kinematics (including blanks) ----------------------------
  dmatch <- 
    dd_match_x %>% 
    mutate(status = factor(status, levels = c("Pre", "Post"))) %>% 
    mutate(type = factor(type, levels = c("Measured", "Matched"))) %>% 
    mutate(
      frow = fct_inorder(str_extract(name, "Pel|Hip|Kne|Ank|Foo")),
      fcol = fct_inorder(str_extract(name, "Cor|Sag|Trn"))
    ) %>% 
    filter(
      !(name %in% c("Ank.Ang.Cor", "Ank.Ang.Trn", "Foo.Ang.Cor"))
    ) %>% 
    mutate(name = fct_drop(name))
  
  # Identify target plots for highlighting -------------------------------------
  dtarget <- 
    dmatch %>% 
    filter(str_detect(name, target)) %>% 
    mutate( value = 0, type = "Measured", Exam_ID = 1, side = "L", status = "Pre") %>%
   mutate(name = fct_drop(name)) %>%  
    distinct()
  
  
  # Build avaraged data (Patient and Match) ------------------------------------
  davg <- 
    bind_rows(dd_match_x_avg, measured_x) %>% 
    mutate(status = factor(status, levels = c("Pre", "Post"))) %>% 
    mutate(type = factor(type, levels = c("Measured", "Matched"))) %>% 
    mutate(
      frow = fct_inorder(str_extract(name, "Pel|Hip|Kne|Ank|Foo")),
      fcol= fct_inorder(str_extract(name, "Cor|Sag|Trn"))
    ) %>% 
    filter(
      !(name %in% c("Ank.Ang.Cor", "Ank.Ang.Trn", "Foo.Ang.Cor"))
    ) %>% 
    mutate(name = fct_drop(name)) 
  
  return(list("davg" = davg, "dtarget" = dtarget, "dmatch" = dmatch))
}