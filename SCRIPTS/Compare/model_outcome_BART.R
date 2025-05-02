# ---- Build categorical outcome models from BART models ----
model_outcome_BART <- 
  function(outbart, df, v){
    # Suppress warnings -----
    options(warn = -1)
    
    # Remove observations with missing y, yPost ----
    df <- df %>% drop_na(all_of(v), all_of(glue("{v}Post")))
    
    # Outcome variables with unknown direction ----
    vunk <- c("ANTEVERSION", "BIMAL", "meansta_Hip_Ang_Trn",
              "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
    # Get predictors ----
    vlist <- names(outbart[[v]]$mod$X)
    
    # Get thresholds ----
    if(!(v %in% vunk)){
      thresh <- get_outcome_thresh(v, df[1,])[1] * get_outcome_thresh(v, df[1,])[2]
      thresh <- rep(thresh, nrow(df))
    } else{
      thresh <-  get_outcome_vunk(df, v)
    }
    
    # Get outcome ----
    diff <- df[[glue("{v}Post")]] - df[[v]]
    
    # ---- Function to comptue outcome based on y and thresh ----
    yfun <- function(diff, thresh){
      thresh.gt.zero <- (thresh > 0)
      diff.gt.thresh <- (diff > thresh)
      test <- case_when(
        thresh > 0 ~ diff > thresh,
        thresh < 0 ~ diff < thresh
      )
      res <- ifelse(test, "Good", "Poor")
      return(res)
    }
    
    df$y <- factor(yfun(diff, thresh), levels = c("Poor", "Good"))
    
    # Choose model ----
    mod <- outbart[[v]]$mod
    temp <- outbart[[v]]$testperf
    X <- mod$X
    vv <- names(X)
    
    # Get data ----
    dat_train <- suppressMessages(inner_join(df, X))
    dat_test <- setdiff(df, dat_train)
    thresh_test <- thresh[dat_test$rowid]
    
    # Percent of "Good" outcome ----
    pct_good <- round(sum(dat_test$y == "Good") / nrow(dat_test), 2)
    
    # Get outcome ----
    temp <- bart_machine_get_posterior(mod, dat_test %>% select(all_of(vv)))
    out_pred <- temp$y_hat
    out_meas <- dat_test[[glue("{v}Post")]] - dat_test[[v]]
    
    # Get outcome > thresh probability ----
    post <- temp$y_hat_posterior_samples
    
    # Get probability of good outcome based on threshold sign ----
    postx <- post * sign(thresh_test)
    y_prob <- rowSums(postx > abs(thresh_test)) / nrow(postx)
    
    pred <- tibble(
      y_meas = dat_test$y,
      # TODO: need to fix this vvvv
      y_pred = factor(y_prob >= .5, levels = c("FALSE", "TRUE"), labels=c("Poor","Good")),
      good_prob = y_prob
    )
    
    # Compute sens, specy, ppv, npv, accuracy using yardstick metrics ----
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=y_meas, estimate=y_pred)
    perf$var <- v
    perf$model <- "BART"
    perf$pct_good <- pct_good
    perf$thresh <- unique(abs(thresh))
    perf <- perf %>% mutate(.estimate = round(.estimate, 2))
    
    # Reinstate warnings ----
    options(warn = 0)
    
    return(list(mod = mod, perf = perf, pred = pred))
  }
