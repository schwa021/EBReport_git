add_prior_surgery <- function(FD, surg){
  
  getprior <- function(ex, side, fd = FD, srg = surg){
    metalist <- unique(srg$SurgCode)
    
    ix <- which(fd$Exam_ID == ex)
    exdate <- fd$Event_Date[ix][1]
    mrn <- fd$Hosp_Num[ix][1]
    
    if(side == "L"){
      s <- srg %>% filter((Hosp_Num == mrn) & (Event_Date <= exdate) & (surgL == 1))
    } else {
      s <- srg %>% filter(Hosp_Num == mrn & Event_Date <= exdate & (surgR == 1))
    }
    
    prior <- data.frame(matrix(nrow = 1, ncol = length(metalist)))
    names(prior) <-  paste0("prior_", metalist)
    
    temp <- s %>% select(SurgCode) %>% .$SurgCode
    
    for (kk in 1:length(metalist)) {
      prior[1, kk] <- sum(temp %in% metalist[kk])
    }
    
    return(prior)
  }
  
  # List all surgery codes, sides, exams
  metalist <- unique(surg$SurgCode)
  sidelist <- FD$SIDE
  examlist <- FD$Exam_ID
  
  
  # loop over all exams and sides with getprior
  priordat <- 
    data.frame(matrix(data = NA, nrow = length(examlist), ncol = length(metalist)))
  
  priordat <- map2_dfr(examlist, sidelist, getprior)

  
  # Build prior surgery data frame
  temp <- FD %>% 
    select(c(Exam_ID, SIDE)) %>% 
    mutate(Exam_ID = factor(Exam_ID))
  prior <- bind_cols(temp, priordat)
  
  
  # Add prior surgery to FD
  FD <- left_join(FD, prior) %>% 
    mutate(across(starts_with("prior_"), ~ ifelse(is.na(.), 0, .))) %>% 
    distinct()
  
  return(FD)
  
}