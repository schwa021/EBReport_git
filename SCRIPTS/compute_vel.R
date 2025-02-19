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
  
  # Check that muscle length data exists -----
  hasdata <- str_subset(names(c$VideoData), "psoas_Z|semimem_Z|rect_fem_Z|add_long_Z|med_gas_Z")
  
  if(length(hasdata) != 10){
    c$VideoData <- 
      c$VideoData %>% 
      mutate(
        Lpsoas_Z = NA_real_, Lsemimem_Z = NA_real_, Lrect_fem_Z = NA_real_, Ladd_long_Z = NA_real_, Lmed_gas_Z = NA_real_,
        Rpsoas_Z = NA_real_, Rsemimem_Z = NA_real_, Rrect_fem_Z = NA_real_, Radd_long_Z = NA_real_, Rmed_gas_Z = NA_real_,
        Lpsoas_v = NA_real_, Lsemimem_v = NA_real_, Lrect_fem_v = NA_real_, Ladd_long_v = NA_real_, Lmed_gas_v = NA_real_,
        Rpsoas_v = NA_real_, Rsemimem_v = NA_real_, Rrect_fem_v = NA_real_, Radd_long_v = NA_real_, Rmed_gas_v = NA_real_
      )
  } else {
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
  }
  
  return(c)
}
