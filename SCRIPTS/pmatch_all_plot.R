pmatch_all_plot <- function(temp){
  
  stat <- unique(temp$davg$status)
  
  tit <- ifelse(stat == "Pre", "Baseline", "Follow-Up")
  
  clr <- ifelse(stat == "Pre", myred, myblue)
  
  pal <- case_when(
    stat == "Pre" ~ c(myred, myred),
    stat == "Post" ~ c(myblue, myblue)
  )
  
  lab <- case_when(
    stat == "Pre" ~ c("Matched Pre", "Measured Pre"),
    stat == "Post" ~ c("Matched Post", "Measured Post")
  )
  
  # Layout for factors
  design <- "
  ABC
  DEF
  GHI
  #J#
  #KL
"
  
  TD <- 
    TD %>% 
    filter(
      !(name %in% c("Ank.Ang.Cor", "Ank.Ang.Trn", "Foo.Ang.Cor"))
    )
  
  p <- 
    ggplot(temp$davg,
           aes(
             x = t,
             y = value,
             group = type,
           )) +
    
     # Layout facets
    facet_manual(vars(name), design = design, scales = "free_y",
                 widths = rep(5, 3), heights = rep(3, 5), respect = TRUE) +
    
    # Add zero line
    geom_hline(yintercept = 0,
               color = "grey20",
               linewidth = .2) +
    
    # Highlight target facets
    geom_rect(data = temp$dtarget,
              fill = "transparent", color = "grey20", linewidth = .4,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    
    # TD Ribbon
    geom_ribbon(dat=TD,
                aes(ymin = lwr, ymax = upr), 
                alpha = .2, color = NA, fill=mygreen, show.legend = F) +
    
    # 90% CI Ribbon
    geom_ribbon(aes(ymin = lwr, ymax = upr), 
                alpha = .2, color = NA, fill=clr, show.legend = F) +
    
    
    # Individual matches
    geom_line(
      data = temp$dmatch,
      aes(
        x = t,
        y = value,
        group = interaction(Exam_ID, side)
      ),
      color = clr,
      linetype = "solid",
      linewidth = .2,
      alpha = .1
    ) +
    
    # Add mean match and patient
    geom_line(aes(linetype = type), linewidth = .5, color = clr) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    
    # labs(title = tit) +
    
    ylab("Value") + 
    xlab("Percent Gait Cycle") +
    guides(
      fill = "none",
      linetype = guide_legend(title = "")) +
    theme(
      legend.position = "bottom"
    )
  
  
  return(p)
  
}