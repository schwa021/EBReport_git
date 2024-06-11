# Reconstruct curve from *PREDICTED* feature scores ----------------------------
get_predlm <- function(d, exam, side, features, nf, pp){
  # Predict nf feature scores (nsim posterior samples) for subject  -----
  dd <- 
    d %>% 
    filter(Exam_IDPost == exam,
           SIDE == side)
  
  fscores <- vector()
  for (kk in 1:nf) {
    mod <- get(glue("mod{kk}"))
    fscores <- 
      fscores %>% 
      rbind(predict(mod, d %>% filter(Exam_IDPost == exam, SIDE == side)))
  }
  
  # Reconstruct curve from nf features -----
  feat <- features[, 1:nf] %>% data.matrix()
  fscores <- fscores[1:nf,] %>% data.matrix()
   
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
    pivot_longer(-c(var, t))
  
  return(f)
}