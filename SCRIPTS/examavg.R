# Group and average ------------------------------------------------------------
examavg <- function(D, TrialInfo, id){
  y <- tibble()
  
  # Get Data for selected Exam_ID
  D <- D[TrialInfo$Exam_ID == id]
  
  # Handle null data
  if(nrow(D %>% list_rbind()) == 0) return(y)
  
  # Compute average per Exam_ID x Side
  y <- 
    D %>%
    list_rbind() %>% 
    group_by(side, t, name) %>% 
    reframe(
      Patient_ID = Patient_ID,
      Exam_ID = Exam_ID,
      Year = Year,
      Age = Age,
      Sex = Sex,
      var = var,
      orthdev = orthdev,
      assdev = assdev,
      # model = model,
      across(
        c(value, OFO, OFC, FO, speed, steplen, mass, leglen, stridelen, oppsteplen), ~ mean(., na.rm = TRUE)
      )
    ) %>% 
    distinct()
  
  return(y)
}
