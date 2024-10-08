# This function builds a propensity model using the BART algorithm and evaluates it on test data.
# s is the surgery of interest, dat is the dataset, and fp2fn is the cost of a false positive relative to a false negative.
# The function returns a list containing the model, the optimal test metrics, and the metrics at each threshold.

build_propensity_mod <- function(s, dat, fp2fn=1){
  
  # This function gets the diagnostic metrics at a given probability threshold -----
  # getres <- function(metrics, prob){
  #   res <-
  #     metrics |>
  #     filter(thresh == prob) |>
  #     mutate(across(everything(),~ round(.,2)))
  #   return(res)
  # }
  
  # This function calculates several diagnostic metrics for a given threshold -----
  calculate_metrics <- function(threshold, data, cost) {
    predicted_positive <- data$probabilities >= threshold
    actual_positive <- data$actual_classes == 1
    prev <- sum(data$actual_classes==1)/nrow(data)
    
    TP <- sum(predicted_positive & actual_positive) |> as.double()
    FP <- sum(predicted_positive & !actual_positive) |> as.double()
    TN <- sum(!predicted_positive & !actual_positive) |> as.double()
    FN <- sum(!predicted_positive & actual_positive) |> as.double()
    
    sens <- TP / (TP + FN)
    spec <- TN / (TN + FP)
    ppv <- TP / (TP + FP)
    npv <- TN / (TN + FN)
    MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    
    return(c(thresh = threshold, acc = (TP + TN) / (nrow(data)), TP = TP, FP = FP,
             TN = TN, FN = FN, ppv = ppv, npv = npv, sens = sens, spec = spec,
             J = sens + spec - 1, d2 = (1 - sens)^2 + ((1-prev)/(cost*prev))*(1 - spec)^2,
             MCC = MCC, FPminusFN = cost * FP - FN))
  }
  
  # Get features for the chosen surgery s -----
  prop_vars <- get_prop_model_vars(s)
  surg <- glue("interval_{s}")
  
  # Define training and testing sets -----
  set.seed(42)
  ex <- unique(dat$Exam_ID)
  extrain <- sample(ex, size = round(.7 * length(ex)))
  dtrain <- dat |> filter(Exam_ID %in% extrain)
  dtest <- dat |> filter(!(Exam_ID) %in% extrain)
  
  # Get balanced dataset for training -----
  ncase <- table(dtrain[[surg]])["1"]
  dtrainbal <- slice_sample(dtrain, n = ncase, by = all_of(surg))
  
  # Make data split -----
  xtrain <- dtrain |> select(all_of(prop_vars)) |> data.frame()
  ytrain <- dtrain |> select(matches(surg)) |> pull(1)
  xtrainbal <- dtrainbal |> select(all_of(prop_vars)) |> data.frame()
  ytrainbal <- dtrainbal |> select(matches(surg)) |> pull(1)
  xtest <- dtest |> select(all_of(prop_vars)) |> data.frame()
  ytest <- dtest |> select(matches(surg)) |> pull(1)
  
  # Train model -----
  mod <-
    bartMachine(xtrainbal, ytrainbal, use_missing_data = T, serialize = T, seed = 42, use_missing_data_dummies_as_covars = T)
  
  # Evaluate model on test data -----
  ytest_prob <- predict(mod, new_data = xtest)
  ytest_pred <- factor(as.numeric(ytest_prob > mod$prob_rule_class), levels = c("1", "0"))
  conf <- table(ytest, ytest_pred)
  
  # Generate metrics for test data -----
  # By default, FP and FN cost are equal (wt_fp2fn = 1)
  data <- data.frame(probabilities = ytest_prob, actual_classes = ytest)
  prev <- sum(ytrain==1)/length(ytrain)
  wt_fp2fn <- 1
  metrics <-
    map(.x = round(seq(0, 1, .01), 2), .f = calculate_metrics, data = data, cost = 1/wt_fp2fn) |>
    bind_rows()
  
  # AUC -----
  f <- splinefun(x = 1 - metrics$spec, y = metrics$sens, ties = mean)
  auc <- integrate(f, 0, 1)$value |> round(2)
  
  # Organize optimal test metrics -----
  opttest <- data.frame(
    prevalence = prev,
    sensitivity = conf["1", "1"] / (conf["1", "1"] + conf["1", "0"]),
    specificity = conf["0", "0"] / (conf["0", "1"] + conf["0", "0"]),
    accuracy = (conf["1", "1"] + conf["0", "0"])/sum(conf),
    ppv = conf["1", "1"] / (conf["1", "1"] + conf["0", "1"]),
    npv = conf["0", "0"] / (conf["1", "0"] + conf["0", "0"]),
    auc = auc
  ) |> 
    round(2)
  
  # Return model and test metrics
  return(list("mod" = mod, "opt" = opttest, "metrics" = metrics))
  
}
