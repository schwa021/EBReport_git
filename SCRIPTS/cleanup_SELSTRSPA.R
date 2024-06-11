## Clean up SEL, STR, SPAS

# The main thing here is to have complete factors, even if there are no observations in a given factor level
# This is makes many future tasks easier (e.g. mo |> deling with BART)

cleanup_SELSTRSPA <- function(FD){
  
  # temp <- FD
  v <- names(FD)
  
  # SEL: -99-->NA, factor, levels = 0, 1, 2
  vx <- str_subset(v, "_SEL$")
  FD <- FD %>% 
    mutate(across(all_of(vx), ~ ifelse(. == -99, NA, .))) %>% 
    mutate(across(all_of(vx), ~ as.factor(.))) %>% 
    mutate(across(all_of(vx), ~ fct_expand(., "0", "1", "2"))) %>% 
    mutate(across(all_of(vx), ~ fct_relevel(., "0", "1", "2"))) %>% 
    mutate(across(all_of(vx), ~ fct_na_value_to_level(., level = "Missing")))
  
  # STR: drop +/-, -99-->NA, factor, levels = 0, 1, 2, 3, 4, 5
  vx <- str_subset(v, "_STR$")
  FD <- FD %>% 
    mutate(across(all_of(vx), ~ str_replace_all(., "[+-]", ""))) %>% 
    mutate(across(all_of(vx), ~ ifelse(. == 99, NA, .))) %>% 
    mutate(across(all_of(vx), ~ as.factor(.))) %>% 
    mutate(across(all_of(vx), ~ fct_expand(., "0", "1", "2", "3", "4", "5"))) %>% 
    mutate(across(all_of(vx), ~ fct_relevel(., "0", "1", "2", "3", "4", "5"))) %>% 
    mutate(across(all_of(vx), ~ fct_na_value_to_level(., level = "Missing")))
  
  # SPAS: drop +/-, -99-->NA, factor, levles = 0, 1, 2, 3, 4
  vx <- str_subset(v, "_SPAS$")
  FD <- FD %>% 
    mutate(across(all_of(vx), ~ str_replace_all(., "[+-]", ""))) %>% 
    mutate(across(all_of(vx), ~ ifelse(. == 99, NA, .))) %>% 
    mutate(across(all_of(vx), ~ as.factor(.))) %>% 
    mutate(across(all_of(vx), ~ fct_expand(., "0", "1", "2", "3", "4"))) %>% 
    mutate(across(all_of(vx), ~ fct_relevel(., "0", "1", "2", "3", "4"))) %>% 
    mutate(across(all_of(vx), ~ fct_na_value_to_level(., level = "Missing")))
  
  
  # Clonus
  FD <- FD %>%
    mutate(ANKLE_CLONUS = fct_lump_min(ANKLE_CLONUS, min = 30, other_level = "Missing")) %>%
    mutate(ANKLE_CLONUS = str_replace(ANKLE_CLONUS, "-99", "Missing")) %>%
    mutate(ANKLE_CLONUS = str_replace(ANKLE_CLONUS, "-", "Absent")) %>%
    mutate(
      ANKLE_CLONUS = case_when(
        str_detect(ANKLE_CLONUS, "U") ~ "Unsustained",
        str_detect(ANKLE_CLONUS, "S") ~ "Sustained",
        TRUE ~ ANKLE_CLONUS
      )
    ) %>%
    mutate(ANKLE_CLONUS = str_replace(ANKLE_CLONUS, ".*\\+", "Unsustained")) %>%
    mutate(ANKLE_CLONUS = factor(
      ANKLE_CLONUS,
      levels = c("Absent", "Unsustained", "Sustained", "Missing")
    ))
  
  return(FD)
  
}