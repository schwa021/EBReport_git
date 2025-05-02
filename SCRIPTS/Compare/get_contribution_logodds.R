# ---- Function to get log-odds ratio contribution scores for Naive Bayes ----
get_contribution_logodds <- 
  function(obss, mod, df_tr, df_te, vlist, s, class_test) {
    surg <- glue("interval_{s}")
    feature_contributions <- numeric(ncol(obss))
    names(feature_contributions) <- names(obss)
    
    for (feature in names(obss)) {
      feature_data <- mod$tables[[feature]]
      obs_value <- obss[[feature]]
      
      # Feature is numeric -----
      if (is.numeric(obs_value)) {
        if (inherits(feature_data$`1`, "density")) {
          # Evaluate the kernel density at the observed value
          if(class_test == 1) {
            feature_data <- feature_data$`1`
          } else {
            feature_data <- feature_data$`0`
          }
          iclose <- which.min(abs(feature_data$x - obs_value))
          
          # If the observed value  outside range (e.g., NA), set likelihood to NA
          if(length(iclose) > 0) {
            likelihood <- feature_data$y[iclose]
          } else {
            likelihood <- NA
          }
          
          # Handle cases where the density might be effectively zero -----
          if (likelihood > 0 & !is.na(likelihood)) {
            feature_contributions[feature] <- log(likelihood)
          } else {
            feature_contributions[feature] <- -Inf
          }
        }
        
        # Feature is categorical -----
      } else if (is.factor(obs_value)) { 
        # Categorical feature (remains the same logic)
        if (as.character(obs_value) %in% rownames(feature_data)) {
          prob <- feature_data[as.character(obs_value), as.character(class_test)]
          if (!is.na(prob) && prob > 0) {
            feature_contributions[feature] <- log(prob)
          } else {
            feature_contributions[feature] <- -Inf
          }
        } else {
          feature_contributions[feature] <- -Inf # Unseen level
        }
      }
    }
    
    # ---- Build contribution data ----
    contribution_df <- 
      tibble(
        feature = vlabs[names(feature_contributions)],
        log_likelihood = feature_contributions
      ) %>% 
      mutate(
        feature = fct_inorder(feature),
        class_test = fct(as.character(class_test), levels = levels(df_te$y)),
      )
    
    return(contribution_df)
  }

