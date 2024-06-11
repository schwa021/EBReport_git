# Reconstruct curve based on *COMPUTED* feature scores -------------------------
get_recon <- function(d, exam, side, features, targetx, nf, pp){
  
  exlhs <- ifelse(pp == "Post", "Exam_IDPost", "Exam_ID")
  featstr <- ifelse(pp == "Post", "^Feat_target.*Post$", "^Feat_target")
  
  # Get measured scores -----
  fscores <- 
    d %>% 
    filter(
      # Exam_ID == {{ex}},
      .data[[exlhs]] == {{exam}},
      SIDE == {{side}}
    ) %>% 
    select(matches(featstr)) %>% 
    data.matrix() %>% 
    t()
  
  # Reconstruct curve from nf features -----
  feat <- features[, 1:nf] %>% data.matrix()
  fscores <- fscores[1:nf,] %>% data.matrix()
  
  # Number of different variables -----
  nvar <- nrow(feat) / 51
  var <- vector()
  for (kk in 1:nvar) {
    var <- c(var, rep(kk, 51))
  }
  
  f <- 
    feat %*% fscores %>% 
    as_tibble() %>% 
    mutate(t = rep(seq(0, 100, 2), nvar)) %>% 
    mutate(var = var) %>% 
    relocate(var, t) %>% 
    rename(value = V1)
  
  return(f)
}