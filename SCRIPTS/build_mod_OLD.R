build_mod <- function(s, dat, fp2fn=1){
  # Get features -----
  prop_vars <- get_model_vars(s)
  surg <- glue("interval_{s}")
  
  
  # Split into training and testing --------------------------------------------
  set.seed(42)
  rnum <- runif(nrow(dat))
  itrain <- between(rnum, 0, .6)
  ival <- between(rnum, .6, .8)
  itest <- between(rnum, .8, 1)
  
  
  # Function to split data -----
  fsplit <- function(dat, ix, prop_vars, surg){
    dout <- dat %>% filter(ix) %>% data.frame() 
    xout <- dout %>% dplyr::select(all_of(prop_vars))
    yout <- dout %>% dplyr::select(matches(surg)) %>% pull(1)
    return(list("d" = dout, "x" = xout, "y" = yout))
  }
  
  
  # Make split -----
  dtrain <- fsplit(dat, itrain, prop_vars, surg)$d
  xtrain <- fsplit(dat, itrain, prop_vars, surg)$x
  ytrain <- fsplit(dat, itrain, prop_vars, surg)$y
  
  dval <- fsplit(dat, ival, prop_vars, surg)$d
  xval <- fsplit(dat, ival, prop_vars, surg)$x
  yval <- fsplit(dat, ival, prop_vars, surg)$y
  
  dtest <- fsplit(dat, itest, prop_vars, surg)$d
  xtest <- fsplit(dat, itest, prop_vars, surg)$x
  ytest <- fsplit(dat, itest, prop_vars, surg)$y
  
  
  # Build model -----
  mod <- bartMachine(xtrain, ytrain, use_missing_data = T, serialize = T, seed = 42)
  
  # Find optimal threshold
  optthresh <- function(mod, x, y, wt) {
    prev <- sum(mod$y == 1) / length(mod$y)
    
    roc_test <-
      pROC::roc(
        response = y,
        predictor = predict(mod, x),
        smooth = F,
        auc = T,
        levels = c("0", "1"),
      )
    
    thresh_test <-
      pROC::coords(
        roc = roc_test,
        x = "best",
        ret = c("threshold", "specificity", "sensitivity", "accuracy", "ppv", "npv"),
        transpose = FALSE,
        best.method = "closest.topleft",
        best.weights = c(wt, prev)
      )
    
    thresh_test$auc = as.numeric(roc_test$auc)
    return(thresh_test)
  } 
  
  
  # Find optimal threshold for spec/sens on **validation** data -----
  opt <- optthresh(mod, x=xval, y=yval, wt=1/fp2fn)
  
  
  # Modify prob_rule_class in model
  mod$prob_rule_class <- opt$threshold
  
  
  # Evaluate model on test data -----
  ytest_pred <- predict(mod, new_data = xtest, type = "class") 
  conf <- table(as.numeric(as.character(ytest_pred)), as.numeric(as.character(ytest)))
  
  opttest <- data.frame(
    threshold = opt$threshold,
    specificity = conf[1,1] / (conf[1,1] + conf[1,2]),
    sensitivity = conf[2,2] / (conf[2,1] + conf[2,2]),
    accuracy = (conf[1,1] + conf[2,2])/sum(conf),
    npv = conf[1,1] / (conf[1,1] + conf[2,1]),
    ppv = conf[2,2] / (conf[1,2] + conf[2,2]),
    auc = optthresh(mod, x=xtest, y=ytest, wt=1/fp2fn)$auc
  )
  
  return(list("mod" = mod, "opt" = opttest))
  
}