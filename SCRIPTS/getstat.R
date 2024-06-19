# General function for extracting and arranging stat

getstat <- function(D, ix, stat, statname){
  # Apply statistic (e.g., mean) across all columns
  temp <- sapply(D[ix,], match.fun(stat))
  temp <- lapply(temp, function(x) ifelse(length(x)==0, NA, x))
  temp <- unlist(temp)
  
  # Organize as tibble
  statval <- tibble(
    value = temp,
    stat = statname,
    key = names(temp)
  )
  
  # Adjust t_ variables for stretched length of stance/swing -----
  if(str_detect(statname, "^t_")) statval$value <- statval$value / (max(ix) - min(ix) + 1)
  
  # Extract side, Exam_ID, stat nmae
  statval <- 
    statval %>% 
    mutate(side = str_split_i(key, "_", 1),
           Exam_ID = str_split_i(key, "_", 2),
           name = str_split_i(key, "_", 3)) %>% 
    select(-key)
  
  return(statval)
}