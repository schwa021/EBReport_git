
plot_development <- function(fd, dat, v, vv = v, cap, devdat) {
  
  # Get name levels -----
  namelevs <- fct_inorder(vlabs[vv])
  
  # Build plot data -----
  pdat <-
    fd$FD |>
    select(age, SIDE, all_of(v)) |>
    pivot_longer(-c(age, SIDE)) |>
    mutate(name = fct_inorder(vlabs[name])) |>
    mutate(
      SIDE = case_when(
        # name == "Dynamic Motor Control (Walking)" ~ NA,
        name == "Functional Assessment Questionnaire Transform" ~ NA,
        str_detect(name, "^GOAL") ~ NA,
        name == "Weight" ~ NA,
        name == "Height" ~ NA,
        TRUE ~ SIDE
      )
    )
  
  # If GDI, take care of (Left)/(Right) versions -----
  if (any(pdat$name == "Gait Deviation Index (Left)")) {
    iL <- pdat$SIDE == "L"
    iR <- !iL
    iGDIL <- pdat$name == "Gait Deviation Index (Left)"
    iGDIR <- pdat$name == "Gait Deviation Index (Right)"
    pdat$value[iL & iGDIR] <- NA
    pdat$value[iR & iGDIL] <- NA
    
    pdat <-
      pdat |>
      drop_na(value) |>
      mutate(
        name = str_replace(name, " \\(Left\\)", ""),
        name = str_replace(name, " \\(Right\\)", "")
      )
    
    pdat$name <- fct_inorder(pdat$name)
  }
  
  # If DMC, take care of (Left)/(Right) versions -----
  if (any(pdat$name == "Dynamic Motor Control Left")) {
    iL <- pdat$SIDE == "L"
    iR <- !iL
    iDMCL <- pdat$name == "Dynamic Motor Control Left"
    iDMCR <- pdat$name == "Dynamic Motor Control Right"
    pdat$value[iL & iDMCR] <- NA
    pdat$value[iR & iDMCL] <- NA
    
    pdat <-
      pdat |>
      drop_na(value) |>
      mutate(
        name = str_replace(name, " Left", " (Walking)"),
        name = str_replace(name, " Right", " (Walking)")
      )
    
    pdat$name <- fct_inorder(pdat$name)
  }
  
  # get appropriate developmental data -----
  dd <- 
    devdat |> 
    filter(
      sex == unique(fd$FD$Sex)
    ) |> 
    pivot_wider(
      names_from = pct,
      values_from = value
    ) |> 
    filter(
      vbl %in% vv
    ) |> 
    mutate(
      name = factor(name, levels = levels(namelevs))
    )
  
  # Get limits from historical data -----
  lims <-
    dat |>
    select(all_of(vv)) |>
    reframe(across(everything(),
                   ~ quantile(
                     ., probs = c(.01, .99), na.rm = T
                   )))
  
  # If GOAL data set lims (0, 100) ------
  if(vv[1] == "TOTAL_Score"){
    lims[1,] <- 0
    lims[2,] <- 100
  }
  
  
  # Generate plot limits -----
  # Note that, in general vv = v, exception for GDI d/t (Left)/(Right)...
  dlims <- tibble(
    name = rep(vv, 2),
    value = c(lims[1,] |> as.numeric(), lims[2,] |> as.numeric()),
    age = as.numeric(NA),
    SIDE = "L"
  ) |>
    mutate(name = vlabs[name]) |>
    mutate(name = factor(name), levels = levels(pdat))
  
  # Set conditional age limits for plotting (25 or pt age) -----
  xmax <- ifelse(max(pdat$age) > 25, Inf, 25)
  
  # Make Plot -----
  p <-
    ggplot(pdat, aes(x = age, y = value, color = SIDE)) +
    
    #TD
    geom_ribbon(
      data = dd,
      aes(ymin=pct05, ymax=pct95),
      fill = "grey80",
      y = NA,
      color = NA,
      alpha = .2
    ) +
    geom_line(
      data = dd,
      aes(y=pct50),
      color = "grey65",
      linetype = "dashed",
      linewidth = .5
    ) +
    
    geom_line(linewidth = .8) +
    geom_point(size = 1.45) +
    geom_point(data = dlims) +
    
    facet_wrap(~ name, scales = "free_y") +
    coord_cartesian(xlim = c(2, xmax), clip = "off") +
    scale_color_discrete_qualitative(
      palette = "Dark 3",
      c1 = 80,
      l1 = 35,
      na.value = "grey35",
      name = "Side",
      breaks = c("L", "R")
    ) +
    labs(
      title = "",
      y = "",
      x = "Age  [yr]",
      caption = cap
    ) +
    theme_mhs(8.5) +
    theme(legend.position = "bottom")
  
  # If GOAL data, remove color/fill
  if(vv[1] == "TOTAL_Score"){
    p <- p + guides(color = "none", fill = "none")
  }
  
  
  return(p)
  
}
