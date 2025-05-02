# ---- Build categorical outcome models with Naive Bayes ----
model_outcome_NB <- 
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
    
    # Get data ----
    dat_train <- suppressMessages(inner_join(df, outbart[[v]]$mod$X))
    dat_test <- setdiff(df, dat_train)
    dat_train <- dat_train %>% drop_na(y, all_of(v))
    dat_test <- dat_test %>% drop_na(y, all_of(v))
    
    # Percent of "Good" outcome ----
    pct_good <- round(sum(dat_test$y == "Good") / nrow(dat_test), 2)
    
    # Build the Naive Bayes model ----
    mod <- 
      naive_bayes(
        y ~ .,
        data = dat_train %>% select(all_of(vlist), y),
        laplace = 0.5
      )
    
    # Evaluate the model ----
    y_pred <- predict(mod, newdata = dat_test)
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,2]
    pred <- tibble(
      y_meas = dat_test$y,
      y_pred = y_pred,
      good_prob = y_prob
    )
    
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=y_meas, estimate=y_pred) %>% select(-.estimator)
    perf$var <- v
    perf$model <- "Naive Bayes"
    perf$pct_good <- pct_good
    perf$thresh <- unique(abs(thresh))
    perf <- perf %>% mutate(.estimate = round(.estimate, 2))
    
    # Reinstate warnings ----
    options(warn = 0)
    
    return(list(mod = mod, perf = perf, pred = pred, df = dat_test))
  }
