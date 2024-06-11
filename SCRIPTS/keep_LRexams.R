keep_LRexams <- function(FD){
  
  FD <- 
    FD %>% 
    arrange(Exam_ID, SIDE)
  
  temp <- 
    FD %>% 
    group_by(Exam_ID) %>% 
    reframe(NL = sum(SIDE == "L"),
            NR = sum(SIDE == "R")) %>% 
    filter(NL == 1 & NR == 1) %>% 
    select(Exam_ID)
  
  FD <- 
    FD %>% 
    filter(Exam_ID %in% temp$Exam_ID)
  
  return(FD)
}