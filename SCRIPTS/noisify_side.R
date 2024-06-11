noisify_side <- function(x, n){
  set.seed(42)
  
  # Get variables to noisify -----
  v <- getnoisevars(x)
  
  # Add noise -----
  z <- addnoise(x, n, v)
  
  # Fix category levels (return to original) -----
  xfact <- sapply(x, is.factor)
  xfact <- names(xfact[which(xfact)])
  ix <- xfact %in% names(z)
  zfact <- xfact[ix]
  # z$era <- x$era
  
  for (v in zfact) {
    levels(z[[v]]) <- levels(x[[v]])
  }
  
  # Find un-noisified variables -----
  names_all <- names(x)
  names_noise <- names(z)
  names_nonoise <- names_all[!(names_all %in% names(z))]
  temp <- select(x, all_of(names_nonoise))
  z <- bind_cols(z, temp)
  
  return(z)
}