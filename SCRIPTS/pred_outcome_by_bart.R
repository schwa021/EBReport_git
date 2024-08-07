# Predict change in outcome variable using BART for isolated surgery s

pred_outcome_by_bart <- function(xx, surglist, s, bartlist, vlabs) {
  
  # Generate virtual twins without (0) and with (1) surgery of interest -----
  surg <- glue("interval_{s}")
  xx0 <- xx
  xx0[ , glue("interval_{surglist}")] <- factor(0, levels = c(1, 0))
  xx1 <- xx0
  xx1[ , surg] <- factor(1, levels = c(1, 0))
  
  # Loop over all outcomes -----
  res <- list()
  for (vv in names(bartlist)) {

    mod <- bartlist[[vv]]$mod
    
    if (!is.null(mod)) {
      set.seed(42)
      # Get posteriors and quantiles for control (0) and treated (1) -----
      post0 <- calc_prediction_intervals(mod, new_data = xx0[names(mod$X)], pi_conf = .90)$all_prediction_samples
      post1 <- calc_prediction_intervals(mod, new_data = xx1[names(mod$X)], pi_conf = .90)$all_prediction_samples
      posttau <- post1 - post0
      
      p <- c(.05, .25, .50, .75, .95)
      plabs <- sprintf("%02d", p*100)
      
      q0 <- as_tibble(t(apply(post0, 1, quantile, probs = p, names = FALSE)))
      q1 <- as_tibble(t(apply(post1, 1, quantile, probs = p, names = FALSE)))
      q <- bind_rows(q0, q1)
      names(q) <- glue("pct{plabs}")
      
      # Compute Cohen's D effect size -----
      effL <- mean(post1[1, ] - post0[1, ])
      effR <- mean(post1[2, ] - post0[2, ])
      sdL <- sd(c(post1[1, ], post0[2, ]))
      sdR <- sd(c(post1[2, ], post0[2, ]))
      DL <- effL/sdL
      DR <- effR/sdR
      
      # Get tau > threshold probability -----
      p_thresh_side <- function(vv, xx, sd){
        posttau_side <- case_when(sd=="L" ~ posttau[1,], sd=="R" ~ posttau[2,])
        xx_side <- case_when(sd=="L" ~ xx[1,], sd=="R" ~ xx[2,])
        thresh <- get_outcome_thresh(vv, xx_side)
        res <- 
          case_when(
            thresh[2] < 0 ~ sum(posttau_side < -thresh[1]) / length(posttau_side),
            thresh[2] > 0  ~ sum(posttau_side > thresh[1]) / length(posttau_side),
            thresh[2] == 0 ~ (sum(posttau_side < -thresh[1]) + sum(posttau_side > thresh[1])) / length(posttau_side)
          )
      }
      
      thresh_lab_side <- function(vv, xx, sd){
        xx_side <- case_when(sd=="L" ~ xx[1,], sd=="R" ~ xx[2,])
        thresh <- get_outcome_thresh(vv, xx_side)
        lab <- 
          case_when(
            thresh[2]==-1 ~ glue("{vlabs[vv]} < {thresh[1]}"),
            thresh[2]==1 ~ glue("{vlabs[vv]} > {thresh[1]}"),
            thresh[2]==0 ~ glue("|{vlabs[vv]}| > {thresh[1]}")
          )
        return(lab)
      }
      
      p_thresh_L <- p_thresh_side(vv, xx, "L")
      thresh_lab_L <- thresh_lab_side(vv, xx, "L") 
      p_thresh_R <- p_thresh_side(vv, xx, "R")
      thresh_lab_R <- thresh_lab_side(vv, xx, "R")
      
      p_thresh <- c(p_thresh_L, p_thresh_R)
      thresh_lab <- c(thresh_lab_L, thresh_lab_R)
      
      # Organize results -----
      pred <- 
        tibble(
          surgname = s,
          side = c("L", "R", "L", "R"),
          surg = c("Control", "Control", "Treated", "Treated"),
          var =  vv,
          D = c(DL, DR, DL, DR),
          tau_gt_thresh = rep(p_thresh, 2),
          tau_gt_label = rep(thresh_lab, 2)
        ) %>% 
        bind_cols(q) %>% 
        arrange(side)
      
      res[[vv]] <- pred
    } else {
      res[[vv]] <- NULL
    }
  }
  
  return(res)
}
