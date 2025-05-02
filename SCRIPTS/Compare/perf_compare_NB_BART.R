# ---- Function to compare BART to Naive Bayes ----
perf_compare_NB_BART <- 
  function(mod_list, s){
    # Get NB and BART predictions -----
    temp <- 
      mod_list[[s]]$dat_test %>% 
      mutate(
        y_BART = factor(ifelse(p_BART > 0.5, 1, 0),levels=c("1", "0")),
        y_Naive = factor(ifelse(p_Naive > 0.5, 1, 0),levels=c("1", "0")),
      ) 
    
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy)
    perf_BART <- mlist(temp, truth=y, estimate=y_BART) %>% mutate(method = "BART")
    perf_Naive <- mlist(temp, truth=y, estimate=y_Naive) %>% mutate(method = "Naive Bayes")
    
    # Combine performance ----
    perf_both <- 
      bind_rows(perf_BART, perf_Naive) %>% 
      filter(.metric %in% c("sens", "spec", "bal_accuracy")) %>% 
      select(-.estimator) %>% 
      mutate(prev = sum(mod_list[[s]]$dat_test$y==1)/nrow(mod_list[[s]]$dat_test)) 
    
    perfplot <- 
      ggplot(perf_both, aes(x=.metric, y=.estimate, fill=method)) +
      geom_col(position = position_dodge(width=.6), width=.6) +
      labs(
        title = glue("{vlabs[s]}"),
        x = "",
        y = ""
      ) +
      coord_cartesian(ylim = c(0.5, 1.0)) +
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
      )
    
    return(list(plt = perfplot, perf = perf_both))
  }
