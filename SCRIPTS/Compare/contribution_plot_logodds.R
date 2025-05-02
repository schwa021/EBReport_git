# ---- Make log-odds feature contribution plot for Naive Bayes ----
contribution_plot_logodds <- 
  function(mod_list, df_list, side, s, ypt_prob){
    pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
    
    # Propensities for header ----
    probL <- glue("{as.character(round(100 * ypt_prob[[s]][1]))}%")
    probR <- glue("{as.character(round(100 * ypt_prob[[s]][2]))}%")
    probside <- ifelse(side == "Left", probL, probR)
    
    # Log-likelihood difference between classes ----
    diffside <- 
      get_pdiff(mod_list, df_list, side, s) %>% 
      mutate(side = glue("{side} ({probside})")) %>% 
      arrange(log_likelihood) %>% 
      mutate(feature = fct_inorder(str_wrap(feature, 25)))
    
    ggplot(diffside, aes(y=feature, x=log_likelihood, fill=log_likelihood)) +
      geom_col() +
      geom_vline(xintercept = 0, color = "grey10", lwd=.8) +
      facet_wrap(~side) +
      labs(x = "Contraindication                  Indication", 
           y = "") +
      scale_x_continuous(
        breaks = seq(-3, 3),
        labels = c("Strong", "Medium", "Weak", "None", "Weak", "Medium", "Strong"),
      ) + 
      coord_cartesian(xlim = c(-3,3)) +
      scale_fill_continuous_diverging(
        palette = "Purple-Green",
        limits = c(-log(10), log(10)),
        labels = c("Strong", "Medium", "Weak", "None", "Weak", "Medium", "Strong"),
        breaks = c(-log(10), -log(5), -log(2), 0, log(2), log(5), log(10)),
        oob = scales::squish
      ) +
      theme_minimal() +
      theme(
        plot.title.position = "plot",
        legend.position = "none",
        axis.text.y = element_text(size = rel(.4)),
        axis.text.x = element_text(size = rel(.4), face = "bold"),
        strip.text = element_text(size = rel(.4), face = "bold"),
        plot.title = element_text(size = rel(.3)),
        axis.title = element_text(size = rel(.4)),
        plot.margin = margin(5, 10, 5, 10),
      )
  }
