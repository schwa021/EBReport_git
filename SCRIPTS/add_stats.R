add_stats <- function(FD, datc3d){
  
  widestats <- function(s, sfx){
    sw <-
      s %>% 
      pivot_wider(names_from = c(stat, name), values_from = value)
    
    # add suffix
    if(nchar(sfx) > 0){
      sw <- 
        sw %>% 
        rename_with(~ paste0(.x,"_", sfx), -c(side, Exam_ID))
    }
    return(sw)
  } 
  
  # Organize stats data
  angstats <- widestats(datc3d$Ang_stats, "") 
  momstats <- widestats(datc3d$Mom_stats, "")
  pwrstats <- widestats(datc3d$Pwr_stats, "")
  # rxnstats <- widestats(datc3d$Rxn_stats, "")
  lenstats <- widestats(datc3d$Len_stats, "len")
  velstats <- widestats(datc3d$Vel_stats, "vel")
  
  FD <- 
    FD %>% 
    left_join(angstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) %>% 
    left_join(momstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) %>%
    left_join(pwrstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) %>%
    # left_join(rxnstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) |> 
    left_join(lenstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) %>% 
    left_join(velstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  
  return(FD)
}