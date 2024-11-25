pd_plot_mhs <- function(s, prop = .20){
  
  # Model and features -----
  mod <- get(paste0("mod_", s))
  feat <- names(mod$X)
  vfact <- names(mod$X)[sapply(mod$X, is.factor)]
  vscale <- names(mod$X)[!(names(mod$X) %in% vfact)]
  # Temporary - get rid of NWB_PF1. Needs to be turned into a factor in RFMP+
  vscale <- str_subset(vscale, "NWB_PF1", negate = T)
  
  
  # Initialize some stuff -----
  pdplot <- list()
  pdmax <- -Inf
  
  # Make plots for scale features -----
  for (v in vscale) {
    pds <- pd_BM(mod, v, prop_data = prop)
    
    pdat <-
      tibble(
        "{v}" := pds$x_j_quants,
        pd = pnorm(pds$bart_avg_predictions_by_quantile),
        lwr = pnorm(pds$bart_avg_predictions_lower),
        upr = pnorm(pds$bart_avg_predictions_upper)
      )
    
    # Update max prob -----
    pdmax <- max(pdmax, pdat$upr)
    
    pdplot[[v]] <- 
      ggplot(pdat, aes(x=.data[[v]], y=pd)) + 
      geom_line(linewidth = .4, color = "grey20") + 
      geom_ribbon(aes(ymin=lwr, ymax=upr), fill="grey50", alpha=.2) +
      geom_point(size = .6) +
      labs(
        x = vlabs[v],
        y = ""
      ) +
      theme(
        axis.text.x = element_text(size = rel(.6)),
        axis.text.y = element_blank(),
        axis.title = element_text(size = rel(.6))
      )
  }
  
  
  
  # Make pdplots for factor variables ----- 
  ix <- runif(nrow(mod$X), min=0, max=1) < prop
  X <- mod$X[ix,]
  
  partial_dependence <- function(mod, X, v, vlevs) {
    n_levels <- length(vlevs)
    pred_values <- matrix(nrow = nrow(X), ncol = n_levels)
    for (i in 1:n_levels) {
      data_temp <- X
      data_temp[[v]] <- vlevs[i]
      pred_values[, i] <- predict(mod, data_temp)
    }
    return(pred_values)
  }
  
  for (v in vfact) {
    cat("Processing", v, "\n")
    
    # Define levels of the factor variable
    vlevs <- levels(X[[v]])
    
    pdvals <- partial_dependence(mod, X, v, vlevs)
    pd <- apply(pdvals, 2, mean, na.rm = T)
    pdsd <- apply(pdvals, 2, sd, na.rm = T) / sqrt(nrow(X))
    
    # Update max prob -----
    pdmax <- max(pdmax, pd+1.96*pdsd)
    
    # Create partial dependence plot
    pdat <- 
      tibble({{v}} := vlevs, pd = pd, err = 1.96*pdsd) |> 
      mutate({{v}} := factor(.data[[v]], levels = vlevs))
    
    if(v %in% c("dx", "dxmod")){
      temp <- 
        pdat |> 
        ggplot(aes(x = .data[[v]], y = pd)) +
        geom_col(fill = "grey50", alpha = 2) +
        geom_linerange(aes(ymin = pd - err, ymax = pd + err), 
                       linewidth = .4, 
                       color = "grey30") +
        geom_text(
          aes(
            # y = pd + err, 
            y = 0,
            label = str_wrap(.data[[v]], width=11),
            angle = 90,
            lineheight = .7
          ),
          color = "white",
          nudge_y = .003,
          vjust = .5,
          hjust = 0,
          size = rel(.8)
        ) +
        labs(
          x = vlabs[v],
          y = ""
        ) +
        theme(
          axis.text = element_blank(),
          axis.title = element_text(size = rel(.6))
        )
    } else {
      temp <- 
        pdat |> 
        ggplot(aes(x = .data[[v]], y = pd)) +
        geom_col(fill = "grey50", alpha = 2) +
        geom_linerange(aes(ymin = pd - err, ymax = pd + err), 
                       linewidth = .4, 
                       color = "grey30") +
        labs(
          x = vlabs[v],
          y = ""
        ) +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = rel(.6)),
          axis.title = element_text(size = rel(.6))
        )
    }
    
    pdplot[[v]] <- temp
    
  }
  
  # Update limits based on pdmax
  # pdmax <- mod$prob_rule_class
  
  for (plt in names(pdplot)) {
    pdplot[[plt]] <- 
      pdplot[[plt]] + 
      coord_cartesian(
        ylim = c(-0.10*pdmax, 1.10*pdmax),
        clip = "off"
      )
  }
  
  
  return(pdplot)
  
}
