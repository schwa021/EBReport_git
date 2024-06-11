outcome_plots_BART <- 
  function(s, outvar, xptsd, dref, vlabs, neglabs, poslabs, prop, out_all) {
    # Color palettes -----
    pal <- diverging_hcl(n = 2, h = c(324, 120), c = 60, l = c(40, 97))
    pallt <- diverging_hcl(n = 2, h = c(324, 120), c = 60, l = c(65, 97))
    palborder <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.7)
    palborder[3] <- "grey50"
    
    # Get propensity color -----
    pcat <-
      cut(
        prop, 
        breaks = c(-Inf, .2, .4, .6, .8, Inf),
        labels = c("Unlikely", "Somewhat Unlikely", "Neither", "Somewhat Likely", "Likely")
      )
    pcol <- palborder[as.numeric(pcat)]
    
    # Outcome variable -----
    outvarlab <- vlabs[outvar]
    outvarPost <- glue("{outvar}Post")
    surg <- glue("interval_{s}")
    neglab <- neglabs[outvar]
    poslab <- poslabs[outvar]
    
    # Organize data for plot -----
    pred <-
      out_all %>%
      filter(surgname == s, var == vv, side == xptsd$SIDE) 
    
    # Get effect size for title -----
    D <-
      pred %>% 
      filter(side == xptsd$SIDE) %>% 
      .$D %>% 
      abs() %>% 
      cut(
        breaks = c(-Inf, .2, .5, .8, 1.2, 2.0, Inf),
        labels = c("NONE", "SMALL", "MEDIUM", "LARGE", "VERY LARGE", "HUGE")
      ) %>% 
      unique()
    
    # Compute x-limits (95% reference data interval)
    delref <- dref[[outvarPost]] - dref[[outvar]]
    xlims <- quantile(delref, probs = c(.01, .99), na.rm = T)
    xlims <- c(floor(xlims[1]), ceiling(xlims[2]))
    
    # Make plot -----
    titside <- ifelse(xptsd$SIDE == "L", "LEFT", "RIGHT")
    p_out <-
      ggplot(pred, aes(y = surg)) +
      geom_vline(
        xintercept = 0,
        color = "grey10",
        linewidth = .5,
        linetype = "2121"
      ) +
      geom_linerange(
        aes(xmin=pct05, xmax=pct95, color=surg),
        linewidth = .65,
        lineend = "round"
      ) +
      geom_linerange(
        aes(xmin=pct25, xmax=pct75, color=surg),
        linewidth = 1.3,
        lineend = "round"
      ) +
      geom_point(
        aes(x=pct50, color=surg, fill=surg),
        size = 1.1
      ) +
      geom_text(
        label = neglab,
        x = xlims[1],
        y = -Inf,
        hjust = 0,
        vjust = -1,
        size = 2.3,
        color = "#005B82",
        fontface = "italic"
      ) +
      geom_text(
        label = poslab,
        x = xlims[2],
        y = -Inf,
        hjust = 1,
        vjust = -1,
        size = 2.3,
        color = "#005B82",
        fontface = "italic"
      ) +
      scale_color_manual(values = c(pal[1], pal[2])) +
      scale_fill_manual(values = c(pallt[1], pallt[2])) +
      coord_cartesian(xlim = xlims) +
      guides(color = "none", fill = "none") +
      labs(
        title = glue("<b>{titside}:</b> <b style='color:{pcol}'>Surgery {pcat}</b>, 
                       Isoloated Treatment Effect Magnitude: {D}"),
        x = glue("\u0394 {outvarlab}"),
        y = ""
      ) +
      theme(
        axis.text.y = element_text(
          angle = 90, 
          hjust = .5,
          vjust = .5,
          margin = margin(0,-2, 0, 0, unit = "lines")
        ),
        plot.title = element_textbox_simple(
          size = rel(.8),
          margin = margin(c(0, 0, 0, 2))
        )
      )
    
    return(p_out)
  }
