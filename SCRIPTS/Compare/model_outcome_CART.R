# ---- Build categorical outcome models with CART ----
model_outcome_CART <- 
  function(df, outbart, v){
    # Remove observations with missing y, yPost ----
    df <- df %>% drop_na(all_of(v), all_of(glue("{v}Post")))
    
    # Outcome variables with unknown direction -----
    vunk <- c("ANTEVERSION", "BIMAL", "meansta_Hip_Ang_Trn",
              "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
    
    # Get predictors ----
    vlist <- names(outbart[[v]]$mod$X)
    
    # Get thresholds ----
    if(!(v %in% vunk)){
      thresh <- get_outcome_thresh(v, df[1,])[1] * get_outcome_thresh(v, df[1,])[2]
    } else{
      thresh <-  get_outcome_vunk(df, v)
    }
    
    # Get outcome ----
    y <- df[[glue("{v}Post")]] - df[[v]]
    y <- case_when(
      thresh > 0 & y < thresh ~ "Poor",
      thresh > 0 & y >= thresh ~ "Good",
      thresh < 0 & y < thresh ~ "Good",
      thresh < 0 & y >= thresh ~ "Poor"
    )
    df$y <- factor(y, levels = c("Poor", "Good"))
    
    # Suppress warnings ----
    options(warn = -1)
    
    # Get data -----
    dat_train <- suppressMessages(inner_join(df, outbart[[v]]$mod$X))
    dat_test <- setdiff(df, dat_train)
    vlist <- names(outbart[[v]]$mod$X)
    nsamp <- nrow(dat_train)
    
    # Percent of "Good" outcome ----
    pct_good <- round(sum(dat_test$y == "Good") / nrow(dat_test), 2)
    
    # Define classifier and workflow -----
    tree_spec <- 
      decision_tree(
        cost_complexity = tune(),  # `cp` in rpart
        tree_depth = tune(),       # `maxdepth` in rpart
        min_n = tune()             # `minsplit` in rpart
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")
    
    # Get data for cross-validation folds -----
    Ns <- sum(dat_train$y=="Good")
    dat_bal <- slice_sample(dat_train, n = Ns, by = y)
    
    dat_cv <-
      dat_train %>%
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
        tree_depth(range = c(2, 6)),
        min_n(range = c(round(nsamp/20), round(nsamp/10))),
        size = 50
      )
    
    tree_results <- 
      tune_grid(
        tree_wf,
        resamples = cv_folds,
        grid = tree_grid,
        metrics = metric_set(bal_accuracy, sens, spec, npv, ppv, f_meas)
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
    rpart.plot(mod, type = 5, varlen = 14, faclen=9, cex = .6, roundint = F)
    
    # Evaluate the model ----
    y_meas  <-  dat_test$y
    y_pred <- predict(mod, newdata = dat_test, type = "class")
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,1]
    dat_test$p_CART <- y_prob
    pred <- tibble(truth=y_meas, pred_class=y_pred, class1_prob=y_prob)
    
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=truth, estimate=pred_class) %>% select(-.estimator)
    perf$var <- v
    perf$model <- "CART"
    perf$pct_good <- pct_good
    perf$thresh <- unique(abs(thresh))
    perf <- perf %>% mutate(.estimate = round(.estimate, 2))
    
    # Reinstate warnings ----
    options(warn = 0)
    
    return(list(mod = mod, perf = perf, pred = pred, df = dat_test))
  }
