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
  if(!is.null(datc3d$Mom_stats))  momstats <- widestats(datc3d$Mom_stats, "")
  if(!is.null(datc3d$Pwr_stats))  pwrstats <- widestats(datc3d$Pwr_stats, "")
  if(!is.null(datc3d$Rxn_stats))  rxnstats <- widestats(datc3d$Rxn_stats, "")
  if(!is.null(datc3d$Len_stats))  lenstats <- widestats(datc3d$Len_stats, "len")
  if(!is.null(datc3d$Vel_stats))  velstats <- widestats(datc3d$Vel_stats, "vel")
  
  FD <- left_join(FD, angstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) 
  if(!is.null(datc3d$Mom_stats))  FD <- left_join(FD, momstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  if(!is.null(datc3d$Pwr_stats))  FD <- left_join(FD, pwrstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  if(!is.null(datc3d$Rxn_stats))  FD <- left_join(FD, rxnstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  if(!is.null(datc3d$Len_stats))  FD <- left_join(FD, lenstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  if(!is.null(datc3d$Vel_stats))  FD <- left_join(FD, velstats, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  
  return(FD)
}