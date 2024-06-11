add_interval_treatment <- function(FDpp){
  
  if(nrow(FDpp) > 1){
    priorPre <- str_subset(names(FDpp), "^(?!.*Post).?prior")
    priorPost <- str_subset(names(FDpp), "prior.*Post")
    rxlist <- str_remove_all(priorPre, "prior_")
    
    pre <- FDpp %>% select(all_of(priorPre))
    names(pre) <- str_remove_all(names(pre), "prior_")
    pre <- pre %>% select(all_of(order(names(pre))))
    
    post <- FDpp %>% select(all_of(priorPost))
    names(post) <- str_remove_all(names(post), "prior_|Post")
    post <- post %>% select(all_of(order(colnames(post))))
    
    interval <- post - pre
    names(interval) <- paste0("interval_", names(interval))
    
    FDpp <- bind_cols(FDpp, interval)
    
    temp <- sapply(FDpp, function(x) is.numeric(x) | is.integer(x))
    nums = names(temp[temp])
    
    FDpp <- FDpp %>% 
      mutate(across(all_of(nums), function(x) ifelse(x == -99, NA, x)))
  } else {
    FDpp <- NULL
  }
  
  return(FDpp)
}