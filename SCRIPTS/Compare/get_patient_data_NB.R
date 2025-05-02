# ---- Format patient data for Naive Bayes ----
get_patient_data_NB <- 
  function(xpt, s){
    vlist <- get_prop_model_vars(s)
    
    # Get patient data -----
    obs <- 
      ptdat$xpt %>% 
      select(all_of(vlist)) %>%
      mutate(
        across(
          where(is.numeric),
          ~ as.double(.)
        )
      ) %>%
      mutate(
        across(
          where(~ !is.numeric(.) && any(is.na(.))),
          as.numeric
        )
      ) %>% 
      mutate(
        across(
          where(is.factor),
          ~ factor(., levels = levels(dat[[cur_column()]]))
        )
      )
  }

pred_pt_surg <- 
  function(mod_list, s, xpt){
    obs <- get_patient_data_NB(xpt, s)
    mod <- mod_list[[s]]$mod
    ypt_prob <- predict(mod, newdata = obs, type = "prob")[, 1]
    return(ypt_prob)
  } 
