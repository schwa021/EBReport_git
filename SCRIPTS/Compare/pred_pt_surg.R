
pred_pt_surg <- 
  function(mod_list, s, xpt){
    obs <- get_patient_data_NB(xpt, s)
    mod <- mod_list[[s]]$mod
    ypt_prob <- predict(mod, newdata = obs, type = "prob")[, 1]
    return(ypt_prob)
  } 