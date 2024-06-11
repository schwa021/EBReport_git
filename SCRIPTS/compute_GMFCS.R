compute_GMFCS <- function(FD){
  
  library(randomForest)
  margin <- ggplot2::margin
  
  gmfcsmod <- readRDS("DATA/GMFCSmod.RDS")
  
  # get person data instead of limb data
  FDx <- FD %>% 
    group_by(Exam_ID) %>% 
    summarize(
      age = mean(age, na.rm = T),
      NDspeed = mean(NDspeed, na.rm = T),
      GDI = mean(GDI, na.rm = T),
      FAQ = first(FAQ),
      WALK_STAIR_WO_RAIL = first(WALK_STAIR_WO_RAIL)
    )
  
  FDx$GMFCS_computed <- predict(gmfcsmod, FDx)
  
  # Munging GMFCS_computed (add "V", "Missing", reorder)
  FDx$GMFCS_computed <- fct_na_value_to_level(FDx$GMFCS_computed, level = "Missing")
  FDx$GMFCS_computed <- fct_expand(FDx$GMFCS_computed, "V")
  FDx$GMFCS_computed <- fct_relevel(FDx$GMFCS_computed, "Missing", after = Inf)
  
  # TODO
  # TODO - NEED TO RECODE GMFCS[NonCP] <- "Missing"
  # TODO
  
  FDx <- FDx %>% select(Exam_ID, GMFCS_computed)
  
  FD <- FD %>% 
    left_join(FDx, by = "Exam_ID") %>% 
    rename(GMFCS_meas = GMFCS)
  
  # Merge measured and computed
  imeas <- FD$GMFCS_meas == "Missing"
  icomp <- FD$GMFCS_computed == "Missing"
  
  FD$GMFCS <- FD$GMFCS_meas
  FD$GMFCS[imeas] <- FD$GMFCS_computed[imeas]
  
  return(FD)
}