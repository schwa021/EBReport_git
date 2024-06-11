add_strideparams <- function(FD, datc3d){
  
  temp <- 
    datc3d$Ang_avg %>% 
    list_rbind() %>% 
    filter(name == "Pel.Ang.Sag") %>% 
    select(Exam_ID, side, OFO, OFC, FO, speed, steplen, leglen) %>% 
    rename(SIDE = side) %>% 
    distinct() %>% 
    mutate(Exam_ID = as.character(Exam_ID))
  
  FD <- 
    FD %>% 
    left_join(temp)
  
  FD <- 
    FD %>% 
    mutate(NDspeed = speed / sqrt(9.81 * leglen),
           NDsteplen = steplen / leglen,
           NDcadence = NDspeed / NDsteplen)
  
  return(FD)
}