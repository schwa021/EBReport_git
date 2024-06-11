# Function to stretch all variables in tibble ----------------------------------
stretchwrap <- function(x, d = 0){
 
  # Check for no data
  if(nrow(x) == 0) return(NULL)
  
  # Function to stretch single variable 50 x 4 = 200 ---------------------------
  stretchone <- function(z, OFO, OFC, FO, d){
    t <- seq(0, 100, 2)
    ns <- 50
    
    # Take derivative if "velocity" is called for
    if(d == 1){
      s <- splinefun(0:50, z)
      z <- s(0:50, deriv = 1)
      b <- signal::butter(2, .33)
      z <- signal::filtfilt(b, x = z)
    }
    
    # Funky stuff, like OFO of 0 and so on
    if(any(is.na(z))) return(rep(NA, 4*ns))
    if(any(is.na(c(OFO, OFC, FO)))) return(rep(NA, 4*ns))
    if(OFO < 4) return(rep(NA, 4*ns))
    
    ds1 <- spline(x = seq(0, OFO-2, 2), y = z[between(t, 0, OFO-2)], n = ns)
    ss <- spline(x = seq(OFO, OFC - 2, 2), y = z[between(t, OFO, OFC-2)], n = ns)  
    ds2 <- spline(x = seq(OFC, FO - 2, 2), y = z[between(t, OFC, FO-2)], n = ns)  
    sw <- spline(x = seq(FO, 100, 2), y = z[between(t, FO, 100)], n = ns)  
    s <- c(ds1$y, ss$y, ds2$y, sw$y)

    return(s)
  }
  
  
  # make data wide and split into L and R --------------------------------------
  xw <- 
    x %>% 
    mutate(OFO = 2 * round(OFO / 2),
           OFC = 2 * round(OFC / 2),
           FO = 2 * round(FO / 2)) %>% 
    mutate(OFC = ifelse(OFC <= OFO, OFO + 2, OFC),
           FO = ifelse(FO <= OFC, OFC + 2, FO)) %>% 
    select(-c(Sex, Age, speed, steplen, mass, Patient_ID, Year, var, leglen, stridelen, oppsteplen, orthdev, assdev)) %>% 
    
    # NOTE: added for multiple BF conditions (e.g., +/- walker)
    distinct() |> 
    
    pivot_wider(names_from = c(Exam_ID, name), values_from = value)
  
  xL <- xw %>% filter(side == "L")
  xR <- xw %>% filter(side == "R")
  
  # Stretch L and R
  sL <- apply(xL %>% select(-c(side, t, OFO, OFC, FO)), 2, stretchone, xL$OFO[1], xL$OFC[1], xL$FO[1], d)
  sR <- apply(xR %>% select(-c(side, t, OFO, OFC, FO)), 2, stretchone, xR$OFO[1], xR$OFC[1], xR$FO[1], d)
  
  
  # Name, organize and return --------------------------------------------------
  vL <- glue("L_{colnames(sL)}")
  vR <- glue("R_{colnames(sR)}")
  xLR <- cbind(sL, sR)
  xLR <- data.frame(xLR)
  names(xLR) <- c(vL, vR)
  
  return(xLR)
  
}