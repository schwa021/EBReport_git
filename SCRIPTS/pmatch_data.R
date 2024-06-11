pmatch_data <- function(s, datx, xside){
  source("SCRIPTS/get_matching_vars.R")
  
  # Organize -----
  mod <- get(glue("mod_{s}"))
  surg <- glue("interval_{s}")
  pvar <- glue("p_{s}")
  
  # Choose distance variables -----
  xtra <-get_outcome_vars(s)
  feat <- c(names(mod$X), xtra)
  feat <- get_matching_vars(s)
  # feat <- c(feat, str_subset(names(datx), "^p_")) # Drop p_ vars (19APR2024)
  
  # Find missing patient data and remove from distance variables -----
  missvars <- feat[is.na(xside[feat])]
  if(length(missvars) > 0){
    distvars <- str_subset(feat, glue_collapse(missvars, "|"), negate = TRUE) 
  } else {
    distvars <- feat
  }
  
  # Remove featurs whose levels don't appear in matching limbs -----
  # e.g., if WBFTPOS=CVL, but CVL not in matching data, then don't use WBFTPOS
  temp <- xside %>% select(all_of(distvars))
  f_fact <- names(temp)[map_vec(temp, is.factor)]
  for (f in f_fact) {
    res <- xside[[f]] %in% levels(datx[[f]])
    if(!res){
      distvars <- str_subset(distvars, f, negate = T)
    }
  }
  
  # Remove patient from matching data -----
  df <- datx %>%
    filter(Exam_ID != xside$Exam_ID) %>%
    drop_na(all_of(c(distvars, pvar)))
  
  # Build matching formula -----
  rhs <- glue_collapse(c(distvars), sep = " + ")
  formdist <- as.formula(glue("target ~ {rhs}"))
  
  # Build caliper -----
  # Caliper expressed in raw units (see matchit options)
  caliper <- c(.1)
  names(caliper) = c(pvar)
  
  # Add target data to end of df -----
  dat0 <-
    df %>% 
    filter(.data[[surg]] == 0) %>% 
    bind_rows(xside) %>% 
    mutate(target = 0)
  dat0$target[(nrow(dat0) - nrow(xside) + 1):nrow(dat0)] <- 1
  
  dat1 <-
    df %>% 
    filter(.data[[surg]] == 1) %>% 
    bind_rows(xside) %>% 
    mutate(target = 0)
  dat1$target[(nrow(dat1) - nrow(xside) + 1):nrow(dat1)] <- 1
  
  # Get the match -----
  set.seed(42)
  nmatch = 40
  
  # Currently not using caliper on propensity (19APR2024)
  m0.out <-
    matchit(
      formula = formdist,
      data = dat0,
      distance = "robust_mahalanobis",
      method = "nearest",
      ratio = nmatch,
      replace = FALSE,
      caliper = NULL,
      std.caliper = FALSE
    )
  
  m1.out <-
    matchit(
      formula = formdist,
      data = dat1,
      distance = "robust_mahalanobis",
      method = "nearest",
      ratio = nmatch,
      replace = FALSE,
      caliper = NULL,
      std.caliper = FALSE
    )
  
  # Extract matched data -----
  temp0 <- 
    dat0 %>% 
    slice(as.numeric(m0.out$match.matrix))
  
  temp1 <- 
    dat1 %>% 
    slice(as.numeric(m1.out$match.matrix))
  
  return(list("dmatch0"=temp0, "dmatch1"=temp1, "m0"=m0.out, "m1"=m1.out))
}

