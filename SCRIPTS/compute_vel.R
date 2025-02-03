compute_vel <- function(c){
  
  # Compute first derivative of spline fit -----
  diff1 <- function(x){
    s <- smooth.spline(t, x)
    v <- predict(s, t, deriv = 1)$y
    return(v)
  }
  
  # Get time vector -----
  vrate <- c$Header$Video_Sampling_Rate
  t <- c$VideoData$Frame / vrate
  
  # Compute velocity of L/R muscles -----
  temp <- 
    c$VideoData %>% 
    select(
      Lpsoas_Z, Lsemimem_Z, Lrect_fem_Z, Ladd_long_Z, Lmed_gas_Z,
      Rpsoas_Z, Rsemimem_Z, Rrect_fem_Z, Radd_long_Z, Rmed_gas_Z,
    ) %>% 
    mutate(
      across(
        .cols = everything(),
        .fns = ~ diff1(.),
        .names = "{.col}_v"
      )
    ) %>% 
    rename_with(
      .cols = everything(),
      .fn = ~ str_replace(.x, "_Z_v", "_v")
    ) %>% 
    select(ends_with("_v"))
  
  # Bind muscle velocity to c$VideoData -----
  c$VideoData <- bind_cols(c$VideoData, temp)
  
  return(c)
}
