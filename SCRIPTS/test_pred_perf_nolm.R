test_pred_perf <- function(s, mod, vy, nsamp, t_or_c, dat, xpt){
  
  # Choose treatment and outcome -----------------------------------------------
  s <- "Femoral_Derotation_Osteotomy"
  surg <- glue("interval_{s}")
  vy <- "meansta_Hip_Ang_Trn"
  vyPost <- glue("{vy}Post")
  mod <- get(glue("mod_{s}"))
  
  
  # Split into training and testing --------------------------------------------
  temp <- 
    dat %>% 
    mutate(y = .data[[vyPost]] - .data[[vy]]) %>% 
    drop_na(y, all_of(names(mod$X)))
  set.seed(42)
  prop_train = 0
  itrain <- runif(nrow(temp)) < prop_train
  dtrain <- temp[itrain, ]
  dtest <- temp[!itrain, ]
  
  
  # Select patients who underwent treatment -----
  t_or_c = 1
  nn <- sum(dtest[[surg]] == t_or_c)
  nsamp <- min(nsamp, nn)
  nsamp = 150
  isamp <- sample(which(dtest[[surg]] == t_or_c), nsamp)
  
  # Number of replicates for noise -----
  nrep = 15
  
  
  # Loop over all patients and compute outcome -----
  dout <- tibble()
  for (kk in 1:nsamp) {
    
    if(round(kk/5) == kk/5) cat("\nsample = ", kk,"\n")
    
    # Assign patient data to x, ypt
    x <- dtest[isamp[kk],]
    ypt <- x[[vyPost]] - x[[vy]]
    
    
    # Add noise to patient data -----
    z <- addnoise(x, nrep, allvars)
    
    # Fix category levels (return to original)
    xfact <- sapply(xpt, is.factor)
    xfact <- names(xfact[which(xfact)])
    ix <- xfact %in% names(zpt)
    zfact <- xfact[ix]
    z$era <- x$era
    for (v in zfact) {
      # zpt[[v]] <- factor(zpt[[v]], levels(xpt[[v]]))
      levels(zpt[[v]]) <- levels(xpt[[v]])
    }
    z$Exam_ID <- x$Exam_ID
    
    # Compute propsensities for zpt -----
    # Find portion of posterior above threshold
    ftemp <- function(x, thresh){
      y <- sum(x > thresh) / length(x)
      return(y)
    }
    
    # Get probability of surgeries -----
    for (ss in surglist) {
      # cat("Working on Surger ", s, "\n")
      # Get model -----
      mod <- get(glue("mod_{ss}"))
      
      # Get posterior propensity distributions -----
      set.seed(42)
      prop_post <- bart_machine_get_posterior(mod, z[names(mod$X)])$y_hat_posterior_samples
      z[[glue("p_{ss}")]] <- apply(prop_post, 1, ftemp, thresh = mod$prob_rule_class)
    }
    
    
    # Get and bind matching data for nrep (noise) replicates -----
    temp <- list()
    cat("Matching ",nrep," replicates \n")
    for (jj in 1:nrep) {
      temp[[jj]] <- 
        pmatch_data(s, dtest, z[jj,]) %>% 
        list_rbind()
    }
    match_data <- list_rbind(temp)
    
    
    # m <- pmatch_data(s, dtest, x)$dmatch1  
    if(t_or_c == 1){
      m <- match_data %>% filter(.data[[surg]] == 1)
    } else  {
      m <- match.data %>% filter(.data[[surg]] == 0)
    }
    
    if(nrow(m) < 25) next
    
    # Linear Model -----
    # Choose predictors
    m_mod <- 
      m %>% 
      mutate(y = .data[[vyPost]] - .data[[vy]]) %>% 
      select(y, names(mod$X)) %>%
      mutate(
        across(
          ends_with(c("SPAS", "SEL", "STR")),
          ~ as.numeric(.)
        )
      ) %>% 
      select(-c(starts_with(c("prior", "dx", "WB", "NWB", "PATELLA", "Era"))))
    
    
    # Bayesian linear regression -----
    # Note: for "production" need chains = 2 or 4
    mdl1 <- stan_lm(
      y ~ .,
      data = m_mod,
      prior = R2(location = .2),
      chains = 1, iter = 1000, seed = 42, refresh = 0
    )
    
    
    # Compute modeled value -----
    if(!any(is.na(x_mod))){
      
      # Predictions -----
      ymatch_lm <- posterior_predict(mdl1)
      ymatch_med <- m[[vyPost]] - m[[vy]]
      # ymatch_x <- posterior_predict(mdl1, newdata = x_mod)
      
      # Quantiles -----
      temp <- apply(ymatch_lm, 2, quantile, probs = c(.05, .25, .50, .75, .95))
      qmatch_lm <- rowMeans(temp)
      qmatch_med <- quantile(ymatch_med, probs = c(.05, .25, .50, .75, .95), na.rm = T)
      # qmatch_x <- quantile(ymatch_x, probs = c(.05, .25, .50, .75, .95), na.rm = T)
      
    } else {
      
      # Predictions -----
      ymatch_lm <- m[[vyPost]] - m[[vy]]
      ymatch_med <- m[[vyPost]] - m[[vy]]
      # ymatch_x <- m[[vyPost]] - m[[vy]]
      
      # Quantiles -----
      qmatch_lm <- quantile(ymatch_lm, probs = c(.05, .25, .50, .75, .95), na.rm = T)
      qmatch_med <- quantile(ymatch_med, probs = c(.05, .25, .50, .75, .95), na.rm = T)
      # qmatch_x <- quantile(ymatch_x, probs = c(.05, .25, .50, .75, .95), na.rm = T)
      
    }
    
    
    # Bind all quantiles -----
    q <- 
      bind_rows(qmatch_lm, qmatch_med) %>% 
      mutate(model = c("lm_match", "med_match")) %>% 
      mutate(y = ypt)
    
    names(q) <- c("lwr", "q1", "ypred", "q3", "upr", "model", "y")
    
    dout <- bind_rows(dout, q)
    
  }
  
  
  # Make calibration plot --------------------------------------------------------
  # Assign point color based on quantile -----
  dd <- 
    dout %>% 
    drop_na() %>% 
    mutate(
      clr = ifelse(between(y, lwr, upr), 1, 0),
      clr = ifelse(between(y, q1, q3), 2, clr)
    )
  
  lims <- c(min(dd$y), max(dd$y))
  jt <- (lims[2] - lims[1])/20
  jt <- 0
  
  
  p_pred_perf <- 
    ggplot(dd, aes(x = ypred, y = y)) + 
    geom_hline(yintercept = 0, color = "grey40", linewidth = .4) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = .4) +
    geom_point(aes(color = factor(clr)), size = 1.5, alpha = .7, shape = 1) +
    geom_abline(intercept = 0, slope = 1) +
    scale_color_manual(values = c("#D7191C", "#A6D96A", "#1A9641")) +
    stat_poly_line(color = "grey5", linewidth = 1, linetype = "dashed", lineend = "round", se = F) +
    stat_poly_eq(mapping = use_label(c("eq"))) +
    ylab(glue("Measured \u0394 {vy}")) + 
    xlab(glue("Predicted \u0394 {vy}")) +
    labs(title = str_replace_all(s,"_", " ")) +
    guides(color = "none") +
    facet_wrap(~model, ncol = 1) +
    theme(
      # strip.text = element_blank(),
      # aspect.ratio = 1,
      plot.title = element_text(size = 11, face = "bold")
    )
  
  return(p_pred_perf)
  
}
