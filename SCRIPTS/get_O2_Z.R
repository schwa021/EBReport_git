get_O2_Z <- function(df){
  
  # Get TD NETND -----
  edat <- read.csv("DATA/tdenergy.csv")
  edat$pwrnet_walk <- edat$pwrgross_walk - edat$pwrgross_sit
  
  # Build 3rd order polynomial model NETND vs. speed -----
  # Compute sd (sig) vs speed using prediction level = .68
  consmod <- lm(pwrnet_walk ~ poly(v, degree = 3), data = edat)
  conf1 = predict(consmod, edat, interval = "prediction", level = .68)
  sig <- (conf1[,3] - conf1[,2])/2
  # sig <- (mean(conf1[, 3] - conf1[, 2])) / 2
  v <- edat$v
  sigspline <- splinefun(v, sig)
  
  temp <- data.frame("v" = round(df$O2_NDspeed, 2))
  pred <- predict(consmod, temp)
  
  df$NETND_OXYCONS_Z <- (df$NETND_OXYCONS - pred) / sigspline(df$O2_NDspeed)
  df$NETND_OXYCONS_PCT <- (df$NETND_OXYCONS - pred) / pred
  
  if("NETND_OXYCONSPost" %in% colnames(df)){
    df$NETND_OXYCONS_ZPost <- (df$NETND_OXYCONSPost - pred) / sigspline(df$O2_NDspeedPost)
    df$NETND_OXYCONS_PCTPost <- (df$NETND_OXYCONSPost - pred) / pred
  }
  
  return(df)
  
}