add_interval_details <- function(FDpp, surg){
  
  if(!is.null(FDpp)){
    source("SCRIPTS/sumsurg.R")
    
    # Compute one patient to get names
    s <-
      sumsurg(
        surg,
        mrn = FDpp$Hosp_Num[1],
        predate = FDpp$Event_Date[1],
        postdate = FDpp$Event_DatePost[1]
      )
    
    # Pre-Allocate computed surgical details
    vnames <- names(s)
    
    temp <-
      data.frame(matrix(
        data = NA,
        nrow = nrow(FDpp),
        ncol = length(vnames)
      ))
    
    names(temp) <- vnames
    
    # If multiple surgery between visits, sum up changes -----
    for (kk in seq(1, nrow(FDpp), 2)) {
       s <-
        sumsurg(
          surg, 
          mrn = FDpp$Hosp_Num[kk], 
          predate = FDpp$Event_Date[kk], 
          postdate = FDpp$Event_DatePost[kk]
        )
      if (nrow(s) == 2) temp[kk:(kk + 1),] <- s
    }
 
    FDpp <- bind_cols(FDpp, temp)
  }
  
  return(FDpp)
}
