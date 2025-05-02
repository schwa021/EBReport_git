# ---- Get log-odds contribution from Naive Bayes (difference in classes) ----
get_pdiff <- 
  function(mod_list, df_list, side, s){
    mod <- mod_list[[s]]$mod
    
    df_tr <- mod_list[[s]]$dat_train
    df_te <- mod_list[[s]]$dat_test
    
    obs <- get_patient_data_NB(xpt, s)
    sideN <- ifelse(side=="Left", 1, 2)
    
    p0 <- get_contribution_logodds(obs[sideN,], mod, df_tr, df_te, vlist, s, 0)
    p1 <- get_contribution_logodds(obs[sideN,], mod, df_tr, df_te, vlist, s, 1)
    
    pdiff <- tibble(
      feature = p0$feature,
      log_likelihood = p1$log_likelihood - p0$log_likelihood,
      side = side
    )
    
    return(pdiff)
  }
