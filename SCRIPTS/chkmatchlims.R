chkmatchlims <- function(dat_match, v, xptside, s, trtgrp, sidestr){
  
  # Get quantiles for numeric variables-----
  vchk <- v[v %in% names(dat_match %>% select(where(is.numeric)))]
  
  if(length(vchk) > 0){
    matchlims <- dat_match %>% 
      select(all_of(vchk)) %>% 
      reframe(
        across(everything(),
               ~ quantile(., probs = c(.05, .25, .75, .95), na.rm = T)
        )
      ) %>% 
      mutate(lim = c("lwr", "q1", "q3", "upr")) %>% 
      pivot_longer(-lim) %>%
      pivot_wider(names_from = lim, values_from = value)
  }
  
  # Check patient within matching limits -----
  chk <- rep(NA, length(v))
  kk = 0
  
  # Loop over variables -----
  for (vv in v) {
    kk <- kk+1
    
    # Factor -----
    if(is.factor(xptside[[vv]]) | is.character(xptside[[vv]])) {
      tmatch <- table(dat_match[[vv]]) %>% as.numeric()
      tpt <- table(xptside[[vv]]) %>% as.numeric()
      del <- which.max(tmatch) - which.max(tpt)
      chk[kk] <- case_when(
        abs(del) > 1 ~ "poor",
        abs(del) == 1 ~ "fair",
        abs(del) == 0 ~ "good"
      )
      if(vv=="dx" & abs(del) > 0) chk[kk] <- "fair"
      if(vv=="dxmod") chk[kk] <- NA
      
      # Scale -----
    } else{
      if(is.na(xptside[[vv]])){
        chk[kk] <- NA
      } else {
        chk[kk] <- case_when(
          !between(xptside[[vv]], matchlims$lwr[matchlims$name==vv], matchlims$upr[matchlims$name==vv]) ~ "poor",
          between(xptside[[vv]], matchlims$lwr[matchlims$name==vv], matchlims$upr[matchlims$name==vv]) ~ "fair",
          between(xptside[[vv]], matchlims$lwr[matchlims$name==vv], matchlims$upr[matchlims$name==vv]) ~ "good",
        )
      }
    }
  }
  
  # Organize Data -----
  chk <- as_tibble(chk)
  chk$Characteristic <- v
  chk <- 
    chk %>% 
    relocate(Characteristic) %>% 
    rename(Within_Limits = value)
  
  # return(tblchk)
  return(chk)
}