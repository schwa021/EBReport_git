# Function to sum surgical changes
sumsurg <- function(surg, mrn, predate, postdate){
  
  # Select pre-post interval
  s <- surg %>%
    filter(Hosp_Num == mrn) %>%
    filter(Event_Date %within% interval(predate, postdate)) %>% 
    arrange(Event_Date)
  
  if(nrow(s) > 0){
    # Set untreated changes to zero
    # Example: 
    # Row does not contain Tibial
    # Cloumns do start with Tibial and contain Change
    s[!str_detect(s$SurgCode, "Tibial"),
      str_detect(names(s), "(?=.*Change)^Tibial")] <- 0
    
    s[!str_detect(s$SurgCode, "Femoral"),
      str_detect(names(s), "(?=.*Change)^Femoral")] <- 0
    
    s[!str_detect(s$SurgCode, "DFEO"),
      str_detect(names(s), "(?=.*Change|.*Shortening)^DFEO")] <- 0
    
    s[!str_detect(s$SurgCode, "FDO DFEO"),
      str_detect(names(s), "(?=.*Change|.*Shortening)^FDO_DFEO")] <- 0
    
    s[!str_detect(s$SurgCode, "Neural"),
      str_detect(names(s), "(?=.*Percent)^Neural")] <- 0
    
    s[!str_detect(s$SurgCode, "Leg_Length"),
      str_detect(names(s), "(?=.*Shortening)Leg_Length")] <- 0
    
    # Sum all changes/shortenings/percents
    # s <- s %>% 
    #   select(c(Side, matches("Change|Percent|Shortening"))) %>% 
    #   group_by(Side) %>% 
    #   summarize(across(everything(), function(x) sum(x)))
    
    # Compute change, first presurg, last postsurg
    temp1 <- s %>%
      group_by(Side) %>%
      summarize(across(matches("Change|Percent|Shortening"), ~ sum(.)))
    
    temp2 <- s %>%
      group_by(Side) %>%
      summarize(across(matches("Presurg"), ~ first(.)))
    
    temp3 <- s %>%
      group_by(Side) %>%
      summarize(across(matches("Postsurg"), ~ last(.)))
    
    suppressMessages(
      s <- temp1 %>% 
        left_join(temp2) %>% 
        left_join(temp3)
    )
    
    # Two sides sorted row-1 = L, row-2 = R
    if(nrow(s) == 1){
      if(s$Side == "L"){
        s[2,] <- NA
      } else {
        s[2,] <- s[1,]
        s[1,] <- NA
      }
    }
  }
  
  s <- s %>% select(-Side)
  
  return(s)
}