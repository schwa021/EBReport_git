# This function finds the mean distance between the target limb and 40 nearest neighbors
# and compares this to the average distance between a limb and its 40 nearest neighbors.
# The distance is the Mahalanobis distance based on the features used in both the propensity
# model and the direct matching outcome model.

limbdist <- function(s, datx, xside, status){
  # Organize -----
  mod <- get(glue("mod_{s}"))
  surg <- glue("interval_{s}")
  pvar <- glue("p_{s}")
  
  # Choose distance variables -----
  xtra <- get_outcome_vars(s)
  feat <- c(names(mod$X), xtra)
  feat <- c(feat, str_subset(names(datx), "^p_"))
  
  # Find missing patient data and remove from distance variables -----
  missvars <- feat[is.na(xside[feat])]
  if(length(missvars) > 0){
    distvars <- str_subset(feat, glue_collapse(missvars, "|"), negate = TRUE) 
  } else {
    distvars <- feat
  }
  
  # Remove featurs whose levels don't appear in matching limbs -----
  # For example, if WBFTPOS = CVL, but CVL is not in matching data
  temp <- xside %>% select(distvars)
  f_fact <- names(temp)[map_vec(temp, is.factor)]
  for (f in f_fact) {
    res <- xside[[f]] %in% levels(datx[[f]])
    if(!res){
      distvars <- str_subset(distvars, f, negate = T)
    }
  }

  # Remove patient from matching data and drop obs. with missing data -----
  df <- 
    datx %>%
    filter(Exam_ID != xside$Exam_ID) %>%
    drop_na(all_of(c(distvars, pvar))) %>% 
    filter(.data[[surg]] == ifelse(status == "treated", 1, 0)) %>% 
    select(all_of(distvars))
  
  distdat <- slice_sample(df, n = min(nrow(df), 1000))
  
  # Add target data to last row -----
  distdat <- 
    distdat %>% 
    bind_rows(xside)

  # Compute distance -----
  rhs <- glue_collapse(c(distvars), sep = " + ")
  ff <- as.formula(glue("~ {rhs}"))
  
  D <- 
    mahalanobis_dist(
      formula = ff,
      data = distdat
    ) %>% 
    apply(., 2, sort) %>%
    as_tibble() %>% 
    tail(-1)
  
  medlimb <- median(D[1:40, ncol(D)][[1]])
  
  # Select closest 40
  D40 <- D[1:40,-ncol(D)]
  med40 <- map(D40, median) %>% unlist() %>% as_tibble()

  p_dist <- 
    ggplot(med40, aes(x=value)) + 
    geom_vline(xintercept = quantile(med40$value, probs = seq(.1, .9, .1)), color="grey60", linewidth=.2, linetype="dotted") +
    geom_vline(xintercept = quantile(med40$value, probs = .5), color="grey30", linewidth=.4) +
    geom_density(adjust=2, fill="grey80", color="grey20", alpha = .5) + 
    geom_vline(xintercept = medlimb, linewidth=.6, linetype="dashed", color="firebrick") +
    labs(
      title = str_replace_all(s, "_", " "),
      x = "Distance",
      y = "Probability Density"
    ) +
    theme(
      panel.grid.major = element_blank()
    )
  
  # Compute percentile -----
  pctl <- round(100 - 100 * sum(med40$value > medlimb) / nrow(med40))
  
  return(list(p_dist = p_dist, pctl = pctl))
}
