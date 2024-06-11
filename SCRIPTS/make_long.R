
make_long <- function(x, kk, typestr, TrialInfo){
  
  if(nrow(x) > 0){
    xlong <- x %>%
      mutate(
        leglen = ifelse(side == "L", TrialInfo$LEG_LENGTH_L[kk], TrialInfo$LEG_LENGTH_R[kk]),
        orthdev = ifelse(x$side == "L", TrialInfo$OrthDev_L[kk], TrialInfo$OrthDev_R[kk]),
        assdev = ifelse(x$side == "L", TrialInfo$AssistDev_L[kk], TrialInfo$AssistDev_R[kk]),
        mass = TrialInfo$WEIGHT[kk]
      ) %>% 
      pivot_longer(
        -c(side, t, cycle, model, OFO, OFC, FO, speed, steplen, stridelen, oppsteplen, mass, leglen, orthdev, assdev)
      ) %>%
      mutate(
        Patient_ID = TrialInfo$Patient_ID[kk],
        Exam_ID = TrialInfo$Exam_ID[kk],
        Sex = TrialInfo$Sex[kk],
        Age = TrialInfo$Age[kk],
        Year = year(TrialInfo$Event_Date[kk]),
        Trial_Num = TrialInfo$Trial_Num[kk],
        TrialType_ID = TrialInfo$TrialType_ID[[kk]],
        var = typestr
      ) %>%
      mutate(
        name = nicenames(name, type = typestr)
      ) %>% 
      relocate(
        Patient_ID, Exam_ID, Trial_Num, Year, Age, Sex, cycle, side, t, name, value, 
        OFO, OFC, FO, speed, steplen, mass, leglen, orthdev, assdev, TrialType_ID, model
      )
    
    if(typestr == "Power") xlong <- xlong %>% filter(!is.na(name))
    
  } else {
    
    xlong = x
    
  }
  
  return(xlong)
  
}