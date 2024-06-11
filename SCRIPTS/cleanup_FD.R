cleanup_FD <- function(FD){
  source("SCRIPTS/cleanup_SELSTRSPA.R")
  source("SCRIPTS/cleanup_Foot.R")
  
  # Convert NaN to NA ----
  is.nan.data.frame <- function(x){
    do.call(cbind, lapply(x, is.nan))
  }
  
  FD[is.nan.data.frame(FD)] <- NA
  
  # Clean up SEL/STR/SPA
  FD <- cleanup_SELSTRSPA(FD)
  
  # Clean up Foot
  FD <- cleanup_Foot(FD)
  
  # Random Cleanup
  FD$HIP_EXT <- FD$THOMAS
  
  return(FD)
}