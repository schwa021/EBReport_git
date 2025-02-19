
get_prop <- function(post, surglist, LR){
  
  ix <- ifelse(LR == "L", 1, 2)
  
  pmean <- 
    surglist %>% 
    map(\(ss) mean(post[[ss]]$y_hat_posterior_samples[ix,])) %>% 
    unlist()
 
  # Compute 5th and 95th and organize. Note "q50" is mean, not median! -----
  prop <- 
    surglist %>% 
    map_df(\(ss) quantile(post[[ss]]$y_hat_posterior_samples[ix,], probs = c(.05, .25, .75, .95))) %>% 
    mutate(Surgery = str_replace_all(surglist, "_", " ")) %>% 
    mutate(SIDE = LR) %>% 
    rename(q5=`5%`, q25=`25%`, q75=`75%`, q95=`95%`) %>% 
    mutate(q50 = pmean) %>% 
    mutate(
      across(
        starts_with("q"),
        ~ round(100 * .)
      )
    )
  
  return(prop)

}
