# Function to get X-learner based treatment effect
# Input is pre-post data (dat), outcome of interest (v), and surgery (surg)
# Returned data includes treatment effect (tau) for each Exam_ID x SIDE
# The treatment effect is named tau__{surg}__{v}

get_tau_X <- function(dat, v, s){
  surg <- glue("interval_{s}")
  
  # Split Data: Treatmend and Control -----
  dat0 <- dat %>% filter(!!sym(surg) == 0)
  dat1 <- dat %>% filter(!!sym(surg) == 1)
  
  # Get outcome models -----
  res0 <- build_pred_outcome_BART(dat0, v, usegait = TRUE, ptrain = 1)
  res1 <- build_pred_outcome_BART(dat1, v, usegait = TRUE, ptrain = 1)
  mod0 <- res0$mod
  mod1 <- res1$mod
  
  # Get Measured Outcome -----
  y <- dat[[glue("{v}Post")]] - dat[[v]]
  
  # Get data ensuring no NA in outcome (y) -----
  datx <- 
    dat %>% 
    select(all_of(c(names(mod0$X), glue("p_{s}")))) %>% 
    bind_cols(y = y) %>%
    drop_na(y)
  
  X <- datx %>% select(-y, -all_of(glue("p_{s}")))
  y <- datx$y
  g <- datx[[glue("p_{s}")]]
  
  # Set surgery to 0 (control) or 1 (treatment) ----
  X0 <- X %>% 
    mutate(across(.cols = all_of(surg), function(x) x = factor("0", levels = c("1", "0"))))
  
  X1 <- X %>% 
    mutate(across(.cols = all_of(surg), function(x) x = factor("1", levels = c("1", "0")))) 
  
  # STEP 1: Estimate the response function
  mu0 <- predict(mod0, X0)
  mu1 <- predict(mod1, X1)
  
  # STEP 2: Impute the treatment effects in Treated and Control separately ----
  # (a) Impute
  D1 <- y - mu0
  D1[X[[surg]] == 0] <- NA
  D0 <- mu1 - y
  D0[X[[surg]] == 1] <- NA
  
  # (b) Fit models tau0 for D0 and tau1 for D1
  x0 <- bind_cols(X, "D0" = D0) %>% drop_na(D0) %>% select(-D0) %>% data.frame()
  y0 <- D0[!is.na(D0)]
  x1 <- bind_cols(X, "D1" = D1) %>% drop_na(D1) %>% select(-D1) %>% data.frame()
  y1 <- D1[!is.na(D1)]
  
  set.seed(42)
  tau0 <- bartMachine(x0, y0, seed = 42, use_missing_data=T, serialize=T,
                      use_missing_data_dummies_as_covars=TRUE)
  tau1 <- bartMachine(x1, y1, seed = 42, use_missing_data=T, serialize=T,
                      use_missing_data_dummies_as_covars=TRUE)
  
  # STEP 3: Treatment effect = weighted sum of predictions from tau0 and tau1 ----
  t0 <- predict(tau0, X)
  t1 <- predict(tau1, X)
  tau <- g * t0 + (1 - g) * t1 
  datx[["outcome"]] <- v
  datx[["surgery"]] <- s
  datx[["tau"]] <- tau
  
  # STEP 4: Add tau to original data ----
  res <- datx %>% 
    left_join(dat) %>% 
    select(Exam_ID, SIDE, outcome, surgery, tau)
  
  return(res)
  
}
