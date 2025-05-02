# ---- BART: Predict outcomes for a given surgery s ----
pred_pt_outcome_BART <- function(xx=xpt, s, out_list_BART){
  # ---- Build control x ----
  x_control <- 
    xx %>% 
    mutate(
      across(
        starts_with("interval_"),
        ~ factor("0", levels = c("1", "0"))
      ),
      status = "Control",
    )
  
  # ---- Build treated x ----
  x_treated <- 
    x_control %>% 
    mutate(
      !!sym(glue("interval_{s}")) := factor("1", levels = c("1", "0")),
      status = "Treated"
    )
  x_sim <- bind_rows(x_treated, x_control)
  
  # ---- Get outcome variables ----
  voutlist <- get_outcome_vars(s)
  
  # ---- Get Treated and Control outcomes for one variable ----
  get_outcome_v1 <- function(out_list_BART, v, x_sim){
    # Choose model ----
    mod <- out_list_BART[[v]]$mod
    X <- mod$X
    
    # Get outcome ----
    temp <- bart_machine_get_posterior(mod, x_sim %>% select(all_of(names(X))))
    out_pred <- temp$y_hat
    post <- temp$y_hat_posterior_samples
    
    # Get threshold for v ----
    threshL <- get_outcome_thresh(v, x_sim[1,])
    threshL <- threshL[1]*threshL[2]
    threshR <- get_outcome_thresh(v, x_sim[2,])
    threshR <- threshR[1]*threshR[2]
    thresh <- c(threshL, threshR, threshL, threshR)
    
    # Assign probability -----
    y_prob <- case_when(
      thresh > 0 ~ rowSums(post > thresh) / ncol(post),
      thresh < 0 ~ rowSums(post < thresh) / ncol(post)
    )
    res <- tibble(
      surgery = vlabs[s],
      var = vlabs[v],
      thresh = thresh,
      status = x_sim$status,
      side = rep(c("Left", "Right"), 2),
      y_value = out_pred,
      good_prob = round(100 * y_prob),
      method = "BART"
    )
  }
  
  # Get outcome for all variables ----
  res <- voutlist %>% 
    map(get_outcome_v1, out_list_BART=out_list_BART, x_sim=x_sim) %>% 
    bind_rows() 
  return(res)
}
