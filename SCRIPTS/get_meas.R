# Get measured curve from Ang_avg ----------------------------------------------
get_meas <- function(dd, ex, sd, target){
  f <- 
    dd %>% 
    filter(
      Exam_ID == {{ex}},
      side == {{sd}}
      # str_detect(name, {{target}})
    ) %>% 
    select(t, name, value)
  
  return(f)
}