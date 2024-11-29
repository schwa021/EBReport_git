source("SCRIPTS/build_pred_outcome_BART.R")
bartlist <- readRDS("DATA/outcome_models_bartlist.RDS")

partial_dependence <- function(mod, X, vbl, pct) {
  xx <- slice_sample(X, prop = pct)
  levs <- levels(X[[vbl]])
  n_levels <- length(levs)
  pred_values <- matrix(nrow = nrow(xx), ncol = n_levels)
  for (i in 1:n_levels) {
    data_temp <- xx
    data_temp[[vbl]] <- factor(levs[i], levels = levels(xx[[vbl]]))
    pred_values[, i] <- predict(mod, data_temp)
  }
  return(pred_values)
}

# "TOTAL_Score", "Activities_Sports_Rec", "ADL_Indep", "Braces_Mobility",
# "Gait_Func_Mobility", "Gait_Pattern_Appearance", "Image_Esteem", 
# "Pain_Discomfort_Fatigue"

get_cause <- function(dat, vv, pct){

  res <- build_pred_outcome_BART(dat, vv)
  mod <- res$mod
  
  pcal <- 
    ggplot(res$testperf, aes(x=pred,y=meas, color=cover, group=1)) + 
    geom_abline(intercept = 0, slope = 1, linetype="dashed", color="grey5") +
    geom_vline(xintercept = 0) + 
    geom_hline(yintercept = 0) + 
    geom_point() + 
    geom_smooth(method = "lm", color="grey5") + 
    labs(y="\u0394 Measured", x="\u0394 Predicted", title = vlabs[vv]) + 
    guides(color = "none") +
    theme_mhs(11)
  
  # ggsave("junk.png", dpi=600, width=7, height=5, bg="white")
  
  # Model and features -----
  vinterval <- str_subset(names(mod$X), "^interval")
  
  # Initialize some stuff -----
  pdplot <- list()
  eff <- list()
  for (s in vinterval) {
    cat("Processing", s, "\n")
    
    # Define levels of the factor variable
    pdvals <- partial_dependence(mod, X=mod$X, vbl=s, pct=pct) %>% as_tibble()
    
    vlevs <- levels(mod$X[[s]])
    vlevsx <- ifelse(vlevs == 1, "Yes", "No")
    
    # compute the mean and standard error of the mean for each column of pdvals -----
    eff[[s]] <- 
      pdvals %>% 
      rename(
        !!sym(vlevsx[1]) := V1,
        !!sym(vlevsx[2]) := V2
      ) %>% 
      mutate(
        Effect = Yes - No
      ) %>% 
      reframe(
        across(
          everything(), 
          list(
            mean = ~ mean(.),
            sd = ~ sd(.)
          )
        )
      ) %>% 
      pivot_longer(cols = everything()) %>% 
      separate(name, into = c("name", "stat"), sep = "_") %>% 
      mutate(
        surg = vlabs[s]
      ) %>% 
      pivot_wider(names_from = stat, values_from = value) %>% 
      mutate(
        lwr = mean - 1.96*sd,
        upr = mean + 1.96*sd
      ) %>% 
      rename(status = name)
  }
  
  eff <- eff %>% list_rbind() 
  
  eff <- eff %>% mutate(surg = str_remove_all(surg, "Interval "))
  
  peff <- 
    ggplot(eff %>% filter(status=="Effect"), aes(x=mean, xmin=lwr, xmax=upr, y=surg)) + 
    geom_vline(xintercept = 0) + 
    # geom_col() +   
    geom_pointrange(position = position_dodge(width=.5)) +
    # coord_cartesian(xlim = c(-10, 10), clip = "off") +
    labs(y="Surgery", x="Causal Effect", title=vlabs[vv]) + 
    # scale_fill_continuous_diverging(rev = TRUE, limits = c(-10, 10), oob = scales::squish) +
    theme_mhs(11)
  
  return(list("pcal"=pcal, "peff"=peff, "res"=res))
}


