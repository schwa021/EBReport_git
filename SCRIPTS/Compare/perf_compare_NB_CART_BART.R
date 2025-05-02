# ---- Function to plot  propensity comparison BART vs. NB vs. CART ----
perf_compare_NB_CART_BART <- 
  function(NBlist, CARTlist, BARTlist, s){
    perf_NB <- 
      NBlist[[s]]$perf %>% 
      mutate(method = "Naive Bayes")
    perf_CART <- 
      CARTlist[[s]]$perf %>% 
      mutate(method = "CART")
    
    # Get BART performance on test data ----
    temp <- tibble(
      p_BART = NBlist[[s]]$dat_test$p_BART,
      y_BART = factor(ifelse(p_BART > 0.5, 1, 0),levels=c("1", "0")),
      y = NBlist[[s]]$dat_test$y
    )
    spec <- yardstick::spec
    sens <- yardstick::sens
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy)
    perf_BART <- 
      mlist(temp, truth=y, estimate=y_BART) %>% 
      mutate(
        method = "BART",
        surgery = s
      )
    
    pltdat <- bind_rows(perf_BART, perf_NB, perf_CART) %>% 
      select(-.estimator) %>% 
      filter(.metric %in% c("sens", "spec", "bal_accuracy")) %>% 
      mutate(
        .metric = recode(.metric, 
                         "bal_accuracy" = "BalAcc",
                         "sens" = "Sens",
                         "spec" = "Spec")
      )
    
    # Make the plot ----
    perfplot <- 
      ggplot(pltdat, aes(x=.metric, y=.estimate, fill=method)) +
      geom_col(lwd=.05, color="grey5", position = position_dodge(width=.6), width=.6) +
      labs(
        title = glue("{vlabs[s]}"),
        x = "",
        y = ""
      ) +
      coord_cartesian(ylim = c(0, 1.0)) +
      scale_fill_discrete_diverging(l2=65) +
      theme_minimal() +
      theme(
        plot.title.position = "plot",
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "lines"),
        legend.spacing.y = unit(0.1, "cm"),
        axis.text.x = element_text(size = rel(.5)),
        axis.text.y = element_text(size = rel(.5), face = "bold"),
        title = element_text(size = rel(.5), face = "bold"),
        plot.margin = margin(5, 10, 0, 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey85", linewidth=.1)
      )
    
    return(list(plt = perfplot, perf = pltdat))
  }
