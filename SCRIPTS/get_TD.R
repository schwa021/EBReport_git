get_TD <- function() {
  td <- read_csv("DATA/ControlKinematicsFMC.csv")
  
  # Fix names --------------------------------------------------------------------
  temp <- str_split_fixed(td$Row, "_", 2)
  var <- temp[,1]
  t <- temp[,2]
  
  var <- glue("Ang_{var}")
  var <- str_replace_all(var, "Tra", "_Trn")
  var <- str_replace_all(var, "Sag", "_Sag")
  var <- str_replace_all(var, "Cor", "_Cor")
  var <- str_replace_all(var, "Knee", "Kne")
  var <- str_replace_all(var, "Pelvis", "Pel")
  var <- str_replace_all(var, "Ankle", "Ank")
  var <- str_replace_all(var, "FootProgress", "Foo")
  var <- str_replace_all(var, "Trunk", "Trk")
  var <- str_sub(var, 5, 11)
  var <- str_replace(var, "_", "_Ang_")
  td$var <- var
  td$t <- t
  td <- 
    td %>% 
    mutate(Row = NULL) %>% 
    relocate(c(var, t)) %>% 
    mutate(t = 2*(as.numeric(t)-1))
  
  
  # Get TD "avg" (lwr, med, upr) -------------------------------------------------
  tdavg <- 
    td %>% 
    pivot_longer(-c(var, t)) %>% 
    mutate(name = str_replace(name, "_L|_R", "")) %>% 
    mutate(name = str_replace(name, "\\d+$", "")) %>% 
    group_by(var, name, t) %>% 
    mutate(
      LRavg = mean(value)
    ) %>% 
    select(-value) %>% 
    rename(value = LRavg) %>% 
    distinct() %>% 
    ungroup() %>% 
    select(-name) %>% 
    group_by(var, t) %>% 
    mutate(
      lwr = quantile(value, probs = .025),
      med = quantile(value, probs = .5),
      upr = quantile(value, probs = .975)
    ) %>% 
    select(-value) %>% 
    distinct() %>% 
    mutate(
      var = str_replace_all(var, "_", "."),
      type = "TD"
    ) %>% 
    rename(
      value = med,
      name = var
    )
  
  return(tdavg)
}