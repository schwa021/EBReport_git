pmatch_target_plot <- function(temp, target){
  
  stat <- unique(temp$davg$status)
  
  clr <- ifelse(stat == "Pre", myred, myblue)
  
  pal <- case_when(
    stat == "Pre" ~ c(myred, myred),
    stat == "Post" ~ c(myblue, myblue)
  )
  
  tit <- ifelse(stat == "Pre", "Baseline", "Follow-Up")
  
  lab <- case_when(
    stat == "Pre" ~ c("Matched Pre", "Measured Pre"),
    stat == "Post" ~ c("Matched Post", "Measured Post")
  )
  
  # Select only target data
  filt_target <- function(x){
    y <- 
      x %>% 
      filter(str_detect(name, target)) %>% 
      mutate(name = fct_drop(name))
  }
  
  TDf <- filt_target(TD)
  davgf <- filt_target(temp$davg)
  dmatchf <- filt_target(temp$dmatch)
  
  #New
  p <- 
    ggplot(davgf,
           aes(
             x = t,
             y = value,
             group = type,
           )) +
    
    facet_wrap( ~ name, ncol = 2, scales = "free_y") +
    
    # Add zero line
    geom_hline(yintercept = 0,
               color = "grey20",
               linewidth = .2) +
    
    # TD Ribbon
    geom_ribbon(dat=TDf,
                aes(ymin = lwr, ymax = upr), 
                alpha = .2, color = NA, fill=mygreen, show.legend = F) +
    
    # 90% CI Ribbon
    geom_ribbon(aes(ymin = lwr, ymax = upr), 
                alpha = .2, color = NA, fill=clr, show.legend = F) +
    
    
    # Individual matches
    geom_line(
      data = dmatchf,
      aes(
        x = t,
        y = value,
        group = interaction(Exam_ID, side)
      ),
      color = clr,
      linetype = "solid",
      linewidth = .3,
      alpha = .1
    ) +
    
    labs(title = tit) +
    
    # Add mean match and patient
    geom_line(aes(linetype = type), linewidth = .8, color = clr) +
    
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