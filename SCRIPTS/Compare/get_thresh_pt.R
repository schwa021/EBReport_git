# ---- Get outcome thresholds for patient ----
get_thresh_pt <- 
  function(voutlist, xx){
    threshlist <- voutlist %>% map(get_outcome_thresh, xx)
    names(threshlist) <- voutlist
    threshlist <- 
      threshlist %>% 
      imap_dfr(~ tibble(name = .y, value1 = .x[1], value2 = .x[2])) %>% 
      rename(val = value1, sign = value2) %>% 
      mutate(thresh = ifelse(sign == 0, val, sign*val)) %>% 
      select(-val, -sign)
    threshL <- setNames(threshlist$thresh, threshlist$name)
  }
