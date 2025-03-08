remove_missing_outcome <- function(out_all){
  outvars <- unique(out_all$var)
  
  outL <- xL %>% select(all_of(outvars))
  outR <- xR %>% select(all_of(outvars))
  
  missL <- is.na(outL)
  missR <- is.na(outR)
  
  out_all <- 
    out_all %>% 
    mutate(
      across(
        matches("pct|^tau"),
        ~ case_when(
          side == "L" & var %in% outvars[missL] ~ NA,
          TRUE ~ .
        )
      )
    ) %>% 
    mutate(
      across(
        matches("pct|^tau"),
        ~ case_when(
          side == "R" & var %in% outvars[missL] ~ NA,
          TRUE ~ .
        )
      )
    )
  
  return(out_all)
}