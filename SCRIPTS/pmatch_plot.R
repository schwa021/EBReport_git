pmatch_plot <- function(s, outvar, datx){
  
  # Organize -----
  outvarPost <- glue("{outvar}Post")
  mod <- get(glue("mod_{s}"))
  surg <- glue("interval_{s}")
  pvar <- glue("p_{s}")
  stit <- str_replace_all(s, "_", " ")
  
  
  #############################################################################
  
  
  # Choose distance variables ----------------------------------------------------
  # Features -----
  distvars <- names(mod$X)
  distvars <- str_subset(distvars, "dxmod|^WB|^NWB", negate = T)
  
  # Check for missing patient data -----
  distvars <- distvars[!is.na(xpt[distvars])]
  
  # Remove patient from matching data, fill missing ------------------------------
  # Note: Only for the matching. Reporting is based on actual data including NA
  datfill <- dat %>% 
    filter(Exam_ID != xpt$Exam_ID) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = T), .)))
  
  
  # Build matching formula -------------------------------------------------------
  rhs <- glue_collapse(c(distvars), sep = " + ")
  formdist <- as.formula(glue("target ~ {rhs}"))
  
  # Add target data to end of datfill --------------------------------------------
  dat0 <-
    datfill %>% 
    filter(.data[[surg]] == 0) %>% 
    bind_rows(xpt) %>% 
    mutate(target = 0)
  dat0$target[nrow(dat0)] <- 1
  
  dat1 <-
    datfill %>% 
    filter(.data[[surg]] == 1) %>% 
    bind_rows(xpt) %>% 
    mutate(target = 0)
  dat1$target[nrow(dat1)] <- 1
  
  
  # Build caliper ----------------------------------------------------------------
  caliper <- c(2.5, .1)
  names(caliper) = c("age", pvar)
  
  
  # Get the match ----------------------------------------------------------------
  set.seed(42)
  nmatch = 50
  
  m0.out <-
    matchit(
      formula = formdist,
      data = dat0,
      distance = "robust_mahalanobis",
      method = "nearest",
      ratio = nmatch,
      replace = FALSE,
      caliper = caliper
    )
  
  m1.out <-
    matchit(
      formula = formdist,
      data = dat1,
      distance = "robust_mahalanobis",
      method = "nearest",
      ratio = nmatch,
      replace = FALSE,
      caliper = caliper
    )
  
  
  temp0 <- 
    dat0 %>% 
    filter(Exam_ID != xpt$Exam_ID) %>% 
    slice(as.numeric(m0.out$match.matrix)) %>% 
    mutate(target = 0)
  
  temp1 <- 
    dat1 %>% 
    filter(Exam_ID != xpt$Exam_ID) %>% 
    slice(as.numeric(m1.out$match.matrix)) %>% 
    mutate(target = 0)
  
  # plot(summary(m0.out), xlim = c(0,2), main = "Control Matches")
  # plot(summary(m1.out), xlim = c(0,2), main = "Treated Matches")
  
  
  #############################################################################
  
  
  # Split into control (0) and treated (1) -----
  ntreat <- nrow(temp1)
  ncontr <- nrow(temp0)
  
  
  # Handle factor outcome -----
  if(class(temp0[[outvar]]) == "factor"){
    
    pre <- table(temp0[[outvar]])
    post <- table(temp0[[outvarPost]])
    pred0 <- 
      bind_rows(pre, post) %>% 
      mutate(
        status = c("Pre", "Post"),
        surg = c(glue("No"), glue("No")),
        var = outvar,
       )
    ptval <- case_when(
      xpt[[surg]] == 1 ~ c(NA, NA),
      TRUE ~ rep(xpt[[outvarPost]], 2)
    )
    pred0$ptval <- ptval
    pred0 <- pivot_longer(pred0, cols = -c(status, surg, var, ptval))
    
    pre <- table(temp1[[outvar]])
    post <- table(temp1[[outvarPost]])
    pred1 <- 
      bind_rows(pre, post) %>% 
      mutate(
        status = c("Pre", "Post"),
        surg = c("Yes", "Yes"),
        var = outvar,
      )
    ptval <- case_when(
      xpt[[surg]] == 0 ~ c(NA, NA),
      TRUE ~ rep(xpt[[outvarPost]], 2)
    )
    pred1$ptval <- ptval
    pred1 <- pivot_longer(pred1, cols = -c(status, surg, var, ptval))
    
    pred <- 
      bind_rows(pred0, pred1) %>% 
      mutate(
        value = as.numeric(value),
        status = factor(status, levels = c("Pre", "Post")),
        surg = factor(surg, levels = c("Yes", "No"))
        )
    
    p_out <-
      ggplot(pred, aes(x=name, y=value, fill=status, group=status, color=status, alpha=status)) + 
      geom_col(width = .7, position = position_dodge2(padding = .1), linewidth = .5) +
      geom_vline(aes(xintercept = ptval), linewidth = 2, color = "firebrick3") +
      facet_wrap(~ surg, ncol = 1,strip.position = "left") +
      scale_fill_manual(values = c("white", "grey10"), name = "Status") + 
      scale_color_manual(values = c("grey80", "grey10"), name = "Status") +
      scale_alpha_manual(values = c(0.2, .75), name = "Status") +
      ylab("") + 
      xlab(outvar) +
      theme(
        axis.text.y = element_blank(),
        strip.text.y.left = element_text(size = 7, angle=0),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.key.size = unit(.15, "inches"),
        aspect.ratio = .25
      )
      
    
  } else {
    
    # Compute outcome quantiles -----
    outy0 <- temp0[[outvarPost]] - temp0[[outvar]]
    outy1 <- temp1[[outvarPost]] - temp1[[outvar]]
    outq0 <- quantile(outy0, probs = c(.05, .25, .50, .75, .95), na.rm = T)
    outq1 <- quantile(outy1, probs = c(.05, .25, .50, .75, .95), na.rm = T)
    
    # Build data -----
    pred <- tibble(
      surg = c("Yes", "No"),
      var = rep(glue("Predicted Change in {outvar}"), 2)
    )
    pred <- bind_cols(pred, bind_rows(outq1, outq0))
    
    # Check for sufficient treated matches -----
    if(ntreat < 20){
      pred[1, 3:7] <- NA
      cap <- "Insufficient number of treated matches"
    } else {
      cap <- ""
    }
    
    # Add patient for historical data -----
    outpt <- xpt[[glue("{outvar}Post")]] - xpt[[glue("{outvar}")]]
    ptval <- case_when(
      xpt[[surg]] == 0 ~ c(NA, outpt),
      TRUE ~ c(outpt, NA)
    )
    pred$ptval <- ptval
    
    # Make plot -----
    p_out <- 
      ggplot(pred, aes(y = surg)) +
      geom_vline(xintercept = 0, color = "grey5", linewidth = .7, linetype="2121") +
      geom_linerange(
        aes(xmin = `5%`, xmax = `95%`),
        linewidth = .5,
        color = "grey20",
        lineend = "round"
      ) +
      geom_linerange(
        aes(x = `50%`, xmin = `25%`, xmax = `75%`),
        linewidth = 2,
        color = "grey50",
        lineend = "round"
      ) +
      geom_point(
        aes(x = `50%`),
        size = 3.5,
        shape = 21,
        color = "grey20",
        fill = "white",
        stroke = 1.7
      ) +
      geom_point(
        aes(x = ptval),
        size = 2,
        shape = 19,
        color = "firebrick3",
        stroke = 1.7
      ) +
      ylab("") + 
      xlab( glue("Predicted Change in {outvar}")) +
      labs(caption = cap) +
      theme(
        # aspect.ratio = .25,
        plot.caption = element_text(size = 6, face="italic", color="grey40")
      )
  }
  
  return(p_out)
  
}

