# Build BART outcome prediction models

# Function to build model and return model + stuff -----
build_pred_outcome_BART <- function(dat, vv, usegait=TRUE){
  # Function to split data by index -----
  fsplit <- function(dat, ix, predvars, outvar){
    outvarPost <- glue("{outvar}Post")
    dout <- dat %>% filter(ix) %>% data.frame() %>% drop_na(all_of(c(outvar, outvarPost)))
    xout <- dout %>% dplyr::select(all_of(pred_ars))
    yout <- dout %>% 
      mutate(
        del = .data[[outvarPost]] - .data[[outvar]]
      ) %>% 
      pull(del)
    return(list("d" = dout, "x" = xout, "y" = yout))
  }
  
  # Function to split data by Exam_ID list -----
  fsplit_Exam <- function(dat, Exlist, predvars, outvar){
    outvarPost <- glue("{outvar}Post")
    dout <- dat %>% filter(Exam_ID %in% Exlist) %>% data.frame() %>% drop_na(all_of(c(outvar, outvarPost)))
    xout <- dout %>% dplyr::select(all_of(predvars))
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
  
  # Split dat 80% - 20% based on Exam_ID -----
  set.seed(42)
  uid <- unique(dat$Exam_ID)
  rnum <- runif(length(uid))
  Exam_ID_train <- uid[between(rnum, 0, .8)]
  Exam_ID_test <- uid[between(rnum, .8, 1)]
  
  # Split into training and testing -----
  set.seed(42)
  rnum <- runif(nrow(dat))
  itrain <- between(rnum, 0, .8)
  itest <- between(rnum, .8, 1)
  
  # Make split -----
  # dtrain <- fsplit(dat, itrain, predvars, vv)$d
  # xtrain <- fsplit(dat, itrain, predvars, vv)$x
  # ytrain <- fsplit(dat, itrain, predvars, vv)$y
  dtrain <- fsplit_Exam(dat, Exam_ID_train, predvars, vv)$d
  xtrain <- fsplit_Exam(dat, Exam_ID_train, predvars, vv)$x
  ytrain <- fsplit_Exam(dat, Exam_ID_train, predvars, vv)$y
  
  # dtest <- fsplit(dat, itest, predvars, vv)$d
  # xtest <- fsplit(dat, itest, predvars, vv)$x
  # ytest <- fsplit(dat, itest, predvars, vv)$y
  dtest <- fsplit_Exam(dat, Exam_ID_test, predvars, vv)$d
  xtest <- fsplit_Exam(dat, Exam_ID_test, predvars, vv)$x
  ytest <- fsplit_Exam(dat, Exam_ID_test, predvars, vv)$y 
  
  # Build model -----
  mod <- bartMachine(xtrain, ytrain, use_missing_data = T, serialize = T, seed = 42)
  
  # Test model -----
  temp <- calc_prediction_intervals(mod, xtest, pi_conf = .90)
  testint <- temp$interval
  testval <- rowMeans(temp$all_prediction_samples)
  
  # Get details
  temp <- dtest %>% select(Exam_ID, SIDE)
  
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


