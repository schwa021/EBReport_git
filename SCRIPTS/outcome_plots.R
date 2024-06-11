outcome_plots <- 
  function(match_data, s, outvar, xptsd, dref, vlabs, neglabs, poslabs, prop) {
    
    pal <- diverging_hcl(n = 2, h = c(324, 120), c = 60, l = c(40, 97))
    pallt <- diverging_hcl(n = 2, h = c(324, 120), c = 60, l = c(60, 97))
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
    mod <- get(glue("mod_{s}"))
    neglab <- neglabs[outvar]
    poslab <- poslabs[outvar]
    
    # Split into control (0) and treated (1) -----
    m0 <- match_data$dmatch0
    m1 <- match_data$dmatch1
    ntreat <- nrow(m1)
    ncontr <- nrow(m0)
    
    # If outcome  is a factor variable -----
    # Note: as of 21APR2024 - I have gotten rid of all categorical outcomes
    # The plots were very non-intuitive, and effect sizes are janky
    if (class(m0[[outvar]]) == "factor") {
      pre <- table(m0[[outvar]])
      post <- table(m0[[outvarPost]])
      pred0 <-
        bind_rows(pre, post) %>%
        mutate(
          status = c("Pre", "Post"),
          surg = c(glue("Control"), glue("Control")),
          var = outvar,
        ) |>
        pivot_longer(cols = -c(status, surg, var))
      
      pre <- table(m1[[outvar]])
      post <- table(m1[[outvarPost]])
      pred1 <-
        bind_rows(pre, post) %>%
        mutate(
          status = c("Pre", "Post"),
          surg = c("Treated", "Treated"),
          var = outvar,
        ) |>
        pivot_longer(cols = -c(status, surg, var))
      
      pred <-
        bind_rows(pred0, pred1) %>%
        mutate(
          value = as.numeric(value),
          status = factor(status, levels = c("Pre", "Post")),
          surg = factor(surg, levels = c("Treated", "Control"))
        )
      
      # Make Plot -----
      p_out <-
        ggplot(pred,
               aes(
                 x = name,
                 y = value,
                 fill = surg,
                 group = status,
                 alpha = status
               )) +
        geom_col(
          width = .7,
          position = position_dodge2(padding = .1),
          linewidth = .3,
          color = "grey30"
        ) +
        facet_wrap( ~ surg, ncol = 1, strip.position = "left") +
        scale_fill_manual(values = rev(pal), name = "Status") +
        scale_alpha_manual(values = c(0.2, .80), name = "Status") +
        labs(
          title = glue("<b style='color:{pcol}'>Surgery {pcat}</b>"),
          x = glue("Pre-Post {outvarlab}"),
          y = ""
        ) +
        
        guides(color = "none",
               fill = "none",
               alpha = "none") +
        theme(
          axis.text.y = element_blank(),
          strip.text.y = element_text(size = rel(1)),
          strip.background = element_rect(fill = "white", color = "white"),
          plot.title = element_textbox_simple(
            size = rel(.8),
            # color = pcol,
            margin = margin(c(0, 0, 0, 2), "pt")
          )
        )
      
    } else {                          # Else if outcome  is a scale variable -----
      
      # Compute outcome by quantiles -----
      get_q <- function(m) {
        ymatch <- m[[outvarPost]] - m[[outvar]]
        q <-
          quantile(ymatch,
                   probs = c(.05, .25, .50, .75, .95),
                   na.rm = T)
        return(q)
      }
      q0 <- get_q(m = match_data$dmatch0)
      q1 <- get_q(m = match_data$dmatch1)
      
      # Compate Cohen's D -----
      y0 <- m0[[outvarPost]] - m0[[outvar]]
      y1 <- m1[[outvarPost]] - m1[[outvar]]
      sdy <- sd(c(y0, y1), na.rm = T)
      del <- mean(y1, na.rm = T) - mean(y0, na.rm = T)
      D <- cut(
        abs(del / sdy), 
        breaks = c(-Inf, .2, .5, .8, 1.2, 2.0, Inf),
        labels = c("NONE", "SMALL", "MEDIUM", "LARGE", "VERY LARGE", "HUGE")
      )
      
      # Make Plot -----
      m0 <- match_data$dmatch0 |> mutate(surg = "Control")
      m1 <- match_data$dmatch1 |> mutate(surg = "Treated")
      m <- bind_rows(m0, m1)
      
      # Build data -----
      pred <- tibble(surg = c("Treated", "Control"),
                     var = rep(glue("Predicted Change in {outvar}"), 2))
      pred <- bind_cols(pred, bind_rows(q1, q0))
      
      # Check for sufficient treated matches -----
      if (ntreat < 15) {
        pred[1, 3:7] <- NA
        cap <- "Low number of treated matches"
      } else {
        cap <- ""
      }
      
      # Compute x-limits (95% reference data interval)
      delref <- dref[[outvarPost]] - dref[[outvar]]
      xlims <- quantile(delref, probs = c(.01, .99), na.rm = T)
      xlims <- c(floor(xlims[1]), ceiling(xlims[2]))
      
      # Make plot -----
      p_out <-
        ggplot(pred, aes(y = surg)) +
        geom_vline(
          xintercept = 0,
          color = "grey10",
          linewidth = .5,
          linetype = "2121"
        ) +
        geom_jitter(
          data = m,
          aes(x = .data[[outvarPost]] - .data[[outvar]], color = surg),
          height = 0,
          pch = "|",
          size = 1.4
        ) +
        ggdist::stat_pointinterval(
          data = m,
          aes(
            x = .data[[outvarPost]] - .data[[outvar]],
            color = surg,
            fill = surg
          ),
          point_interval = "mean_qi",
          .width = c(.5, .9),
          shape = 21,
          point_size = 2,
          interval_size_range = c(.5, 1.5),
          stroke = 1
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
        coord_cartesian(xlim = xlims, clip = "on") +
        guides(color = "none", fill = "none") +
        labs(
          title = glue("<b style='color:{pcol}'>Surgery {pcat}</b>, 
                       Isoloated Treatment Effect Magnitude: {D}"),
          x = glue("\u0394 {outvarlab}"),
          y = "",
          caption = cap
        ) +
        theme(
          axis.text.y = element_text(
            angle = 90, 
            hjust = .5,
            vjust = .5,
            margin = margin(0,-2, 0, 0, unit = "lines")),
          plot.title = element_textbox_simple(
            size = rel(.8),
            # color = pcol,
            margin = margin(c(0, 0, 0, 2), "pt")
          )
        )
    }
    
    return(p_out)
    
  }
