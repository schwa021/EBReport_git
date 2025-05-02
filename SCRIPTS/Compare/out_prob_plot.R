# ---- Compare outcome probab distributions between Naive Bayes and Bart ----
out_prob_plot <- 
  function(out_list_BART, out_list_Naive, vtest){
    
    pb <- out_list_BART[[vtest]]$pred$good_prob
    yb <- out_list_BART[[vtest]]$pred$y_meas
    mb <- "BART"
    pn <- out_list_Naive[[vtest]]$pred$good_prob
    yn <- out_list_Naive[[vtest]]$pred$y_meas
    mn <- "Naive Bayes"
    
    tb <- tibble(p=pb, y=yb, m=mb) %>% arrange(p) %>% mutate(id = row_number())
    tn <- tibble(p=pn, y=yn, m=mn) %>% arrange(p) %>% mutate(id = row_number())
    pdat <- bind_rows(tb, tn)
    
    ggplot(pdat %>% drop_na(), aes(x=id, y=p, color=y)) + 
      geom_jitter(shape=1,height=.05, size=.7) + 
      geom_hline(yintercept = .5) +
      facet_wrap(~m) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title=vlabs[vtest], x="", y="Probability of Good Outcome") +
      scale_x_discrete(labels = NULL) +
      scale_color_discrete_diverging(palette = "Purple-Green", l1 = 45, c1 = 60,
                                     guide = guide_legend(title = "Outcome")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(.5), face = "bold"),
        axis.title = element_text(size = rel(.5), face = "bold"),
        axis.text = element_text(size = rel(.5)),
        strip.text = element_text(size = rel(.5), face = "italic"),
      )
  }
