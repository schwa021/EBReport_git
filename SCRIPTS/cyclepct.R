# Function to get event frames in 51 point fit
cyclepct <- function(Scyc){
  Sk <- round(51 * (Scyc - Scyc$FC1) / (Scyc$FC2 - Scyc$FC1))
  Sk$cycle <- seq(1:nrow(Sk))
  return(Sk)
}