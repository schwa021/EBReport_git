add_Historical <- function(FD, Historical.tbl, Delivery_lookup){
  
  temp <- Historical.tbl %>% 
    select(sort(names(Historical.tbl))) %>% 
    select(-c(starts_with("AssistDev")))
  
  lookup <- Delivery_lookup %>% 
    mutate(Description = str_replace(Description, "Unknown", "Missing")) %>% 
    select(-IsUnknown)
  
  temp <- left_join(temp,lookup) %>% 
    rename(Delivery = Description) %>% 
    select(-c(Delivery_ID)) %>% 
    mutate(across(everything(), ~ ifelse(. == -99, NA, .))) %>% 
    mutate(across(everything(), ~ ifelse(. == "U", "Missing", .)))
  
  FD <- left_join(FD, temp)
  
  return(FD)
}