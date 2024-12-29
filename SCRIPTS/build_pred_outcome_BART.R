# Build BART outcome prediction models

# Function to build model and return model + stuff -----
build_pred_outcome_BART <- function(dat, vv, usegait=TRUE){
  # Function to split data -----
  fsplit <- function(dat, ix, pred_vars, outvar){
    outvarPost <- glue("{outvar}Post")
    dout <- dat %>% filter(ix) %>% data.frame() %>% drop_na(all_of(c(outvar, outvarPost)))
    xout <- dout %>% dplyr::select(all_of(pred_vars))
    yout <- dout %>% 
      mutate(
        del = .data[[outvarPost]] - .data[[outvar]]
      ) %>% 
      pull(del)
    return(list("d" = dout, "x" = xout, "y" = yout))
  }
  
  # Get features -----
  predvars <- get_pred_model_vars(vv)
  predvars <- c("age", "GMFCS", predvars, glue("interval_{surglist}"))
  predvars <- unique(predvars)
  
  
  ################################################
  predvars_nogait <- 
    predvars %>% 
    str_subset(
      regex("^ic|^fo|^ofo|^ofc|^mean|^min|^max|^rom|^mid|^t_|DMC|GDI", ignore_case=FALSE), 
      negate = TRUE
    )
  
  if(!usegait){
    predvars <-predvars_nogait
  }
  ################################################
  
  # Split into training and testing -----
  set.seed(42)
  rnum <- runif(nrow(dat))
  itrain <- between(rnum, 0, .8)
  itest <- between(rnum, .8, 1)
  
  # Make split -----
  dtrain <- fsplit(dat, itrain, predvars, vv)$d
  xtrain <- fsplit(dat, itrain, predvars, vv)$x
  ytrain <- fsplit(dat, itrain, predvars, vv)$y
  
  dtest <- fsplit(dat, itest, predvars, vv)$d
  xtest <- fsplit(dat, itest, predvars, vv)$x
  ytest <- fsplit(dat, itest, predvars, vv)$y
  
  # Build model -----
  mod <- bartMachine(xtrain, ytrain, use_missing_data = T, serialize = T, seed = 42)
  
  # Test model -----
  temp <- calc_prediction_intervals(mod, xtest, pi_conf = .90)
  testint <- temp$interval
  testval <- rowMeans(temp$all_prediction_samples)
  
  # Get details
  temp <- fsplit(dat, itest, c("Exam_ID", "SIDE"), vv)$x
  
  testperf <- tibble(
    Exam_ID = temp$Exam_ID,
    SIDE = temp$SIDE,
    meas = ytest,
    pred = testval,
    lwr = testint[,1],
    upr = testint[,2]
  ) %>% 
    mutate(cover = between(meas, lwr, upr))
  
  return(list(mod = mod, testperf = testperf))
}


