remove_missing_outcome_pt <- function(out_list, out_pt, xx=xpt){
  outvars <- unique(names(out_list))
  
  xL <- xx %>% filter(SIDE == "L")
  xR <- xx %>% filter(SIDE == "R")
  
  outL <- xL %>% select(all_of(outvars))
  outR <- xR %>% select(all_of(outvars))
  
  missL <- is.na(outL)
  missR <- is.na(outR)
  
  out_pt <- 
    out_pt %>% 
    mutate(
      across(
        matches("y_value|good_prob"),
        ~ case_when(
          side == "Left" & var %in% vlabs[outvars[missL]] ~ NA,
          TRUE ~ .
        )
      )
    ) %>% 
    mutate(
      across(
        matches("y_value|good_prob"),
        ~ case_when(
          side == "Right" & var %in% vlabs[outvars[missL]] ~ NA,
          TRUE ~ .
        )
      )
    )
  
  return(out_pt)
}