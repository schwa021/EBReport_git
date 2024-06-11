# Find optimal threshold
optthresh <- function(mod, xtest, ytest) {

  roc_test <-
    pROC::roc(
      response = ytest,
      predictor = predict(mod, xtest),
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
      best.method = "closest.topleft"
    )
  
  return(thresh_test)
} 