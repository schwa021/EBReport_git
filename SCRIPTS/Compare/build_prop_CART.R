# ---- Function to build propensity model using CART ----
build_prop_CART <- 
  function(df, prop_list_BART, s){
    # Define interval surgery name -----
    interval <- glue("interval_{s}")
    
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
    nsamp <- nrow(dat_train)
    
    dat_test <- dat_test %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    
    # Define classifier and workflow -----
    tree_spec <- 
      decision_tree(
        cost_complexity = tune(),  # `cp` in rpart
        tree_depth = tune(),       # `maxdepth` in rpart
        min_n = tune()             # `minsplit` in rpart
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")  # Use "regression" for numeric outcomes
    
    # Get data for cross-validation folds -----
    Ns <- sum(dat_train[[interval]]=="1")
    dat_bal <- slice_sample(dat_train, n = Ns, by = all_of(interval))
    
    dat_cv <-
      dat_bal %>%
      select(all_of(vlist), y)
    cv_folds <- vfold_cv(dat_cv, v = 10) 
    
    # Build workflow -----
    tree_wf <- 
      workflow() %>%
      add_model(tree_spec) %>%
      add_formula(y ~ .)
    
    # Grid Search: Random search is fast and works with flat cost -----
    set.seed(42)
    tree_grid <- 
      grid_random(
        cost_complexity(range = c(-4, 0)),
        tree_depth(range = c(2, 4)),
        min_n(range = c(round(nsamp/20), round(nsamp/10))),
        size = 50
      )
    
    tree_results <- 
      tune_grid(
        tree_wf,
        resamples = cv_folds,
        grid = tree_grid,
        metrics = metric_set(sens, spec, bal_accuracy, ppv, npv, f_meas)
      )
    
    # Choose optimal parameters -----
    mopt <- "bal_accuracy"
    best_params <- select_best(tree_results, metric = mopt)
    final_tree <- finalize_model(tree_spec, best_params)
    final_wf <- 
      workflow() %>%
      add_model(final_tree) %>%
      add_formula(y ~ .)
    
    # Train final model ----
    final_fit <- fit(final_wf, data = dat_cv)
    mod <- final_fit %>% extract_fit_engine()
    
    # Plot final model -----
    rpart.plot(mod, type = 5, extra = 104, varlen = 14, faclen=9, cex = .6, roundint = F)
    
    # Evaluate the model ----
    y_pred <- predict(mod, newdata = dat_test, type = "class")
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,1]
    dat_test$p_CART <- y_prob
    pred <- tibble(truth=dat_test$y, pred_class=y_pred, class1_prob=y_prob)
    
    # Compute sens, spec, ppv, npv, accuracy using yardstick metrics ----
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=truth, estimate=pred_class)
    perf$surgery <- s
    perf$.estimate <- round(perf$.estimate, 2)
    
    # Return warnings and result ----
    options(warn = 0)
    
    return(list(mod=mod,perf=perf,dat_train=dat_train, dat_test=dat_test))
  }
