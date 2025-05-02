# ---- Function to build propensity model using Naive Bayes ----
build_prop_NB <- 
  function(df, prop_list_BART, s){
    # Suppress warnings ----
    options(warn = -1)
    
    # Get data -----
    dat_train <- suppressMessages(inner_join(df, prop_list_BART[[s]]$X))
    dat_test <- setdiff(df, dat_train)
    vlist <- names(prop_list_BART[[s]]$X)
    
    dat_train <- dat_train %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    dat_test <- dat_test %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    
    # Build the Naive Bayes model -----
    mod <- 
      naive_bayes(
        y ~ .,
        data = dat_train %>% select(all_of(vlist), y),
        usekernel = TRUE, usepoisson = FALSE, laplace = 1
      )
    
    # Evaluate the model ----
    y_pred <- predict(mod, newdata = dat_test)
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,1]
    dat_test$p_Naive <- y_prob
    pred <- tibble(truth=dat_test$y, pred_class=y_pred, class1_prob=y_prob)
    
    # Compute sens, spec, ppv, npv, accuracy using yardstick metrics ----
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy)
    perf <- mlist(pred, truth=truth, estimate=pred_class)
    perf$surgery <- s
    perf$.estimate <- round(perf$.estimate, 2)
    
    # Return warnings and result ----
    options(warn = 0)
    
    return(list(mod=mod,perf=perf,dat_train=dat_train, dat_test=dat_test))
  }
