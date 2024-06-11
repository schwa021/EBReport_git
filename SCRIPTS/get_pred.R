# Reconstruct curve from *PREDICTED* feature scores ----------------------------
get_pred <- function(d, exam, side, features, nsim, nf, pp){
  # Predict nf feature scores (nsim posterior samples) for subject  -----
  fscores <- vector()
  for (kk in 1:nf) {
    mod <- get(glue("mod{kk}"))
    fscores <- 
      fscores %>% 
      rbind(predict_scores(d, exam, side, mod, nsim, pp))
  }
  
  # Reconstruct curve from nf features -----
  feat <- features[, 1:nf] %>% data.matrix()
  fscores <- fscores[1:nf,] %>% data.matrix()
  if(nrow(fscores) > ncol(fscores)) fscores <- t(fscores)
  
  # Determine number of distinct feature elements (e.g., angles) -----
  nvar <- nrow(feat) / 51
  var <- vector()
  for (kk in 1:nvar) {
    var <- c(var, rep(kk, 51))
  }
  
  # Compute and organize -----
  f <- 
    feat %*% fscores %>% 
    as_tibble() %>% 
    mutate(t = rep(seq(0, 100, 2), nvar)) %>% 
    mutate(var = var) %>% 
    relocate(var, t) %>% 
    pivot_longer(-c(var, t)) %>% 
    group_by(var, t) %>% 
    reframe(
      lwr = quantile(value, probs = .025),
      med = quantile(value, probs = .50),
      upr = quantile(value, probs = .975)
    )
  
  return(f)
}