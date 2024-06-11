# Spline fit data to 51 points and return y-values
makespline <- function(x){
  s <- spline(x, n = 51)
  return(s$y)
}