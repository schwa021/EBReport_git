build_prepost <- function(FD){
  
  #function to generate pre-post data
  getpp <- function(id, fd=FD){
    # Select patient
    pt <- fd %>% 
      filter(Hosp_Num == id) %>% 
      arrange(Event_Date)
    
    # All exams
    exams <- unique(pt[["Exam_ID"]])
    
    # Organize as Pre/Post
    expp <- data.frame(exam_pre = exams[1:(length(exams)-1)], 
                       exam_post = exams[2:length(exams)])
    
    pre <- pt %>% filter(Exam_ID %in% expp$exam_pre)
    post <- pt %>% filter(Exam_ID %in% expp$exam_post)
    
    # Rename post variables
    names(post) <- paste0(names(post), "Post")
    
    # Bind post to pre
    pt_pp <- bind_cols(pre, post)
    
    return(pt_pp)
  }
  
  
  # Get list of Hosp_Num with >1 exams
  idlist <- FD %>% 
    group_by(Hosp_Num) %>% 
    arrange(Event_Date) %>% 
    summarize(nexam = n()/2) %>% 
    filter(nexam > 1) %>% 
    .$Hosp_Num
  
  FDpp <- map(.x = idlist, ~ getpp(.x))
  
  FDpp <- FDpp %>% list_rbind()
  
  return(FDpp)
}