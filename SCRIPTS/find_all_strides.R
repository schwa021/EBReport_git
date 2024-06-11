find_all_strides <- function(c){
  
  Lcyc <- NULL
  Rcyc <- NULL
  
  events <- c$GaitCycleEvents
  
  if(class(events) == "character") return(list("Lcyc" = Lcyc, "Rcyc" = Rcyc))
  
  # Get L/R Cycle start and end frames -----------------------------------------
  FSL <- tibble("event" = "FSL", "frame" = events$FootStrikeFrame_L)
  FOL <- tibble("event" = "FOL", "frame" = events$FootOffFrame_L)
  FSR <- tibble("event" = "FSR", "frame" = events$FootStrikeFrame_R)
  FOR <- tibble("event" = "FOR", "frame" = events$FootOffFrame_R)
  
  # List  all event data -------------------------------------------------------
  evtdat <- bind_rows(FSL, FOL, FSR, FOR) %>% arrange(frame)
  
  # initialize number of cycles on L and R sides
  N_Lcyc = 0
  N_Rcyc = 0
  
  # String sequence defining complete cycle on L and R sides -------------------
  Lstr <- c("FSL", "FOR", "FSR", "FOL", "FSL")
  Rstr <- c("FSR", "FOL", "FSL", "FOR", "FSR")
  Lcyc <- tibble("FC1" = vector(), "OFO" = vector(), "OFC" = vector(), "FO" = vector(), "FC2" = vector())
  Rcyc <- tibble("FC1" = vector(), "OFO" = vector(), "OFC" = vector(), "FO" = vector(), "FC2" = vector())
  
  # Loop over all event data in chunks of 5 events  matching (FS1, OFO, OFC, FO, FS1)
  for (kk in 1:(nrow(evtdat) - 4)) {
    # Left side
    if (all(Lstr == evtdat$event[kk:(kk + 4)])) {
      N_Lcyc <- N_Lcyc + 1
      Lcyc <-
        add_row(
          Lcyc,
          FC1 = evtdat$frame[kk],
          OFO = evtdat$frame[kk + 1],
          OFC = evtdat$frame[kk + 2],
          FO = evtdat$frame[kk + 3],
          FC2 = evtdat$frame[kk + 4]
        )
    }
    
    # Right side (note - repeated code but only once d/t L/R)
    if (all(Rstr == evtdat$event[kk:(kk + 4)])) {
      N_Rcyc <- N_Rcyc + 1
      Rcyc <- add_row(
        Rcyc,
        FC1 = evtdat$frame[kk],
        OFO = evtdat$frame[kk + 1],
        OFC = evtdat$frame[kk + 2],
        FO = evtdat$frame[kk + 3],
        FC2 = evtdat$frame[kk + 4]
      )
    }
  }
  
  return(list("Lcyc" = Lcyc, "Rcyc" = Rcyc))
  
}