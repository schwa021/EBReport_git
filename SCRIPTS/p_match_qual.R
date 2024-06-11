p_match_qual <- function(match_data, xpt, mod){
  
  d0 <- match_data$dmatch0
  d1 <- match_data$dmatch1
  
  # Examine matching of scale variables -----
  vlist <- names(mod$X)[unlist(lapply(mod$X, is.numeric))]
  
  f <- function(dd, xpt, grp, vlist){
    res <- tibble()
    
    D <- bind_rows(dd, xpt)
    D$target[nrow(D)] = 1
    
    for (vv in vlist) {
      temp <- 
        D %>% 
        group_by(target) %>% 
        reframe(
          val = mean(.data[[vv]], na.rm = T),
          var = vv
        )
      res <- bind_rows(res, temp)
    }
    
    res <- 
      res %>% 
      pivot_wider(names_from=target, values_from=val) %>% 
      mutate(grp = grp)    
  }
  
  res0 <- f(d0, xpt, 0, vlist)
  res1 <- f(d1, xpt, 1, vlist)
  
  res <- 
    bind_rows(res0, res1) %>% 
    pivot_longer(-c(var, grp)) %>% 
    mutate(grp = factor(grp, levels = c(1, 0), labels = c("Yes", "No"))) %>% 
    mutate(name = factor(name, labels = c("Matches", "Patient"))) %>% 
    mutate(var = fct_rev(fct_inorder(var)))
  
  p <- ggplot(res, aes(y=var, x=value, group=name, fill=name)) + 
    geom_col(position = position_dodge()) +
    facet_wrap(~grp, ncol = 2) +
    theme(
      axis.text.x = element_text(angle = 0, hjust=1)
    ) +
    ylab("Value") +
    xlab("Variable") +
    labs(title = str_replace_all(s, "_", " ")) +
    guides(fill = guide_legend(title = "")) +
    theme(
      strip.text = element_text(size = 8)
    )
  
  return(p)
  
}