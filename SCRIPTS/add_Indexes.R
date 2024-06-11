add_Indexes <- function(FD, Patient.tbl, Patient_Events.tbl, Exam.tbl, Services.tbl,
                        TMH.tbl, Trial_Type.tbl, Physician_lookup){
  
  TMH <- Patient.tbl %>% 
    left_join(Patient_Events.tbl) %>% 
    left_join(Exam.tbl) %>% 
    left_join(Services.tbl) %>% 
    left_join(TMH.tbl) %>% 
    left_join(Trial_Type.tbl) %>% 
    filter(TrialType_ID == 17)
  
  temp <- TMH %>% 
    select(c(Exam_ID, starts_with("GDI"), starts_with("DMC"))) %>% 
    group_by(Exam_ID) %>% 
    summarize_all(mean, na.rm = TRUE)
  
  # Fill in conventional GDI where functional not available
  ix <- is.na(temp$GDI_L)
  temp$GDI_L[ix] <- temp$GDI_L_Conv[ix]
  temp$GDI_R[ix] <- temp$GDI_R_Conv[ix]
  
  # Average L/R sides
  temp$GDI <- (temp$GDI_L + temp$GDI_R)/2
  temp$DMC <- (temp$DMC_L + temp$DMC_R)/2
  
  GDI_DMC <- temp %>% 
    select(sort(names(temp))) %>% 
    relocate(GDI, DMC)
  
  FD <- FD %>% 
    left_join(GDI_DMC)
  
  # Add physician
  FD <- FD %>% 
    left_join(Physician_lookup)
  
  return(FD)
  
}