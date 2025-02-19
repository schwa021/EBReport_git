propgraph <- function(propL, propR, side){
  
  ncolor <- 5
  # original -----
  pal <- diverging_hcl(n = ncolor, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  # slightly lighteer -----
  pal <- diverging_hcl(n = ncolor, h = c(324, 120), c = 60, l = c(53, 97), power = 1.8)
  
  # Organize propensities
  pL <- 
    propL %>% 
    mutate(
      Surgery = fct_rev(fct_inorder(Surgery)),
      across(
        starts_with("q"),
        ~ ./100
      )
    )
  
  pR <- 
    propR %>% 
    mutate(
      Surgery = fct_rev(fct_inorder(Surgery)),
      across(
        starts_with("q"),
        ~ ./100
      )
    )
  
  prop <- bind_rows(pL, pR) %>% mutate(SIDE = ifelse(SIDE=="L", "Left", "Right"))
  
  # Create a data frame for the gradient
  gradient_df <- expand.grid(
    x = seq(0, 1, length.out = ncolor),  # Generate 100 points for a smooth gradient
    y = prop$Surgery[1:12] # Covers full y-axis range
  )
  gradient_df$fill_color <- gradient_df$x  # Map fill to x position
  
  p <- 
    ggplot(prop, aes(y=Surgery)) + 
    # geom_tile(data = gradient_df, aes(x = x, y = y, fill = fill_color)) + 
    # scale_fill_gradientn(colors = pal) + 
    # Background color panels ---
    geom_rect(aes(xmin=0, xmax=.20, ymin=.5, ymax=12.5), fill=pal[1], alpha=1) +
    geom_rect(aes(xmin=.20, xmax=.40, ymin=.5, ymax=12.5), fill=pal[2], alpha=1) +
    geom_rect(aes(xmin=.40, xmax=.60, ymin=.5, ymax=12.5), fill=pal[3], alpha=1) +
    geom_rect(aes(xmin=.60, xmax=.80, ymin=.5, ymax=12.5), fill=pal[4], alpha=1) +
    geom_rect(aes(xmin=.80, xmax=1.00, ymin=.5, ymax=12.5), fill=pal[5], alpha=1) +
    
    # Reference lines ---
    geom_hline(aes(yintercept = Surgery), color = "grey80", lwd = 0.2, lty = "dotted") +
    geom_vline(xintercept = 0.5, color = "grey30", lwd = 0.4, lty = "dashed") +
    
    # Intervals and points ---
    geom_linerange(aes(xmin=q5, xmax=q95), lwd = .6, color = "grey30", lineend = "round") +
    geom_linerange(aes(xmin=q25, xmax=q75), lwd = 1.4, color = "grey10", lineend = "round") +
    geom_point(aes(x=q50), shape=21, size=1.6, stroke=1, fill="grey90", color="grey10") +
    
    # Add heading ---
    annotate("text", 
             x=c(.3, .7), y = c(13, 13), 
             label = c("No Surgery", "Yes Surgery"), 
             size=2.5, vjust=1, fontface="italic") +

    # Facet by side ---
    facet_wrap(~SIDE) +
    
    # Custom x-axis ---
    scale_x_continuous(
      limits=c(0, 1), breaks=seq(.1, .9, .2), 
      labels = c("High", "Moderate", "Low", "Moderate", "High")
      ) +
    
    labs(
      x="Confidence in Treatment Recommendation\n(Historical Standard of Practice)",
      y="Surgery") +
    theme_mhs(bs=10) +
    theme(
      axis.title.y=element_blank(),
      axis.text.y=element_text(size=7, hjust = 0),
      axis.title.x=element_text(size=9, face="bold"),
      axis.text.x=element_text(size=7),
      plot.title=element_text(size=14),
      strip.text = element_text(size = 9, face="bold"),
      panel.spacing.x = unit(0.5, "lines"),
      plot.title.position = "plot",
      legend.position = "none"
    )
  
  return(p)
}

