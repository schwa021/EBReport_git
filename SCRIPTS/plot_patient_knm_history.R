plot_patient_knm_history <- function(datc3d, fd, xpt, sd, ptname, ptdate, ptmrn) {
  
  # ptname <- glue("{xpt$Child_First} {xpt$Child_Last}")[1]
  # ptdate <- format(as.Date(params$Event_Date), "%b %d, %Y")
  # ptmrn <- xpt$MRN[1]
  tit <- glue("{ptname} ({ptmrn})")
  
  ageym <- function(x){
    xy <- floor(x)
    xm <- round((x - xy) * 12)
    res <- glue("{xy}y + {xm}m")
    return(res)
  }
  
  # Build kinematics (add date)
  temp <- fd$FD |> 
    select(Exam_ID, Event_Date, age) |> 
    distinct() |> 
    mutate(
      Exam_ID = as.integer(Exam_ID),
      Ageym = ageym(age)
    ) |> 
    arrange(age) |> 
    mutate(Ageym = fct_inorder(Ageym))
  
  knmavg <-
    datc3d$Ang_avg |>
    list_rbind() |>
    filter(side == sd) |> 
    inner_join(temp)
  
  nvisit <- length(unique(knmavg$Exam_ID))

  hue <- ifelse(sd == "L", 346, 173)
  clr <- sequential_hcl(
    nvisit,
    h = hue,
    c = 70,
    l = c(30, 75),
    power = 1.2
  )
  clr <- rev(clr)
 
  # Set Line Widths ------
  lw <- rep(.5, nvisit)
  lw[length(lw)] <- .9

  #Build y limits and direction labels -----
  datlwr <-
    tibble(name = unique(knmavg$name),
           value = c(
             c(-15, -15, -15),
             c(-15, -15, -15),
             c(-15, -15, -30),
             c(-15, -30, -45),
             c(-30, -75, -60)
           ))
  datupr <-
    tibble(name = unique(knmavg$name),
           value = c(
             c(30, 60, 30),
             c(30, 75, 30),
             c(15, 75, 15),
             c(15, 30, 30),
             c(15, 30, 30)
           ))
  
  datlwr$labneg <- c(
    "Down", "Post", "Extern", 
    "Abd",  "Exten", "Extern", 
    "Val", "Exten","Extern", 
    "Ever", "Plantar", "Extern",
    "Ever", "Equ", "Extern"
  )
  
  datupr$labpos <- c(
    "Up", "Ant", "Intern", 
    "Add","Flex", "Intern", 
    "Var","Flex", "Intern", 
    "Inver", "Dorsi", "Intern",
    "Inver", "Calc", "Intern"
  )
  
  datlims <-
    bind_rows(datlwr, datupr) |>
    mutate(t = as.numeric(NA),
           side = as.character(NA))
  
  
  # Fix tdavg name levels -----
  tdavg$name <- factor(tdavg$name, levels = levels(knmavg$name))
  
  # Remove coronal plane ankle and foot kinematics -----
  remove_ang <- function(x){
    y <- 
      x |> 
      filter(!(name %in% c("Ank.Ang.Cor", "Foo.Ang.Cor"))) |> 
      mutate(name = fct_drop(name))
    
    return(y)
  }
  
  knmavg <- remove_ang(knmavg)
  tdavg <- remove_ang(tdavg)
  datlims <- remove_ang(datlims)
  
  # Build design for facets (eliminate coronal plane ankle/foot) -----
  design <- "
ABC
DEF
GHI
#JK
#LM
"
  
  # Build plot -----
  plt <-
    ggplot(data = knmavg,
           mapping = aes(x = t, y = value)) +
    geom_hline(yintercept = 0,
               linewidth = .5,
               color = "grey30") +
    geom_ribbon(
      data = tdavg |> filter(!str_detect(name, "Trk")),
      mapping = aes(
        ymin = lwr,
        ymax = upr,
        color = NA,
        group = NA
      ),
      fill = "grey80",
      color = "transparent",
      alpha = .4
    ) +
    geom_line(
      mapping = aes(
        group = Event_Date,
        color = factor(Ageym),
        linewidth = factor(Ageym)
      )
    ) +
    geom_point(data = datlims,
               mapping = aes(
                 x = t,
                 y = value
               ),
    ) +
    
    coord_cartesian(clip = "off") +
    geom_text(
      data = datlims,
      aes(label = labneg, y = value),
      x = -19,
      group = 1,
      angle = 90,
      vjust = 0,
      hjust = 0,
      size = 1.7,
      color = "grey10"
    ) +
    geom_text(
      data = datlims,
      aes(label = labpos, y = value),
      x = -19,
      group = 1,
      angle = 90,
      vjust = 0,
      hjust = 1,
      size = 1.7,
      color = "grey10"
    ) +
    
    # facet_wrap(~ name, ncol = 3, scales = "free_y") +
    ggh4x::facet_manual(~name, design = design, scales = "free_y") +
    
    scale_color_manual(values = clr, name = "Age") +
    scale_linewidth_manual(values = lw, name = "Age") +
    # guides(linewidth = "none") +
    scale_y_continuous(breaks = seq(-90, 90, by = 15)) +
    labs(
      title = glue("{ptname} ({ptmrn}), {sd}-Side"),
      x = "Percent Gait Cycle",
      y = ""
    ) +
    theme_mhs(bs = 6) +
    theme(
      # aspect.ratio = .7,
      axis.text = element_text(size = rel(.75)),
      panel.spacing.x = unit(30, "pt"),
      panel.spacing.y = unit(12, "pt"),
      legend.position = "bottom",
      legend.text = element_text(size = rel(1)),
      legend.title = element_text(size = rel(1)),
      plot.margin = margin(10, 10, 10, 20)
    )
  
  return(plt)
  
}
