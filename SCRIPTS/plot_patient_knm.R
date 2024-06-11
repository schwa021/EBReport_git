plot_patient_knm <- function(datc3d, xpt, ptname, ptdate, ptmrn) {
  # ptname <- glue("{xpt$Child_First} {xpt$Child_Last}")[1]
  # ptdate <- format(as.Date(params$Event_Date), "%b %d, %Y")
  # ptmrn <- xpt$MRN[1]
  tit <- glue("{ptname} ({ptmrn}), Exam Date: {ptdate}")
  
  knm <- 
    datc3d$Ang |> 
    list_rbind() |> 
    filter(Exam_ID == xpt$Exam_ID[1]) |> 
    mutate(grp = interaction(cycle, side, Trial_Num))
  
  knmavg <- 
    datc3d$Ang_avg |>
    list_rbind() |> 
    filter(Exam_ID == xpt$Exam_ID[1])
  
  datlwr <-
    tibble(
      name = unique(knmavg$name),
      value = c(c(-15, -15, -15), c(-15, -15, -15), c(-15, -15, -30), c(-15, -30, -45), c(-30, -75, -60))
    )
  datupr <-
    tibble(
      name = unique(knmavg$name),
      value = c(c(30, 60, 30), c(30, 75, 30), c(15, 75, 15), c(15, 30, 30), c(15, 30, 30))
    )
  
  # Build y-direction labels
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
    mutate(
      t = as.numeric(NA),
      side = as.character(NA)
    )
  
  # Fix tdavg name levels
  tdavg$name <- factor(tdavg$name, levels = levels(knmavg$name))
  
  # Remove coronal plane ankle and foot kinematics -----
  remove_ang <- function(x){
    y <- 
      x |> 
      filter(!(name %in% c("Ank.Ang.Cor", "Foo.Ang.Cor"))) |> 
      mutate(name = fct_drop(name))
    
    return(y)
  }
  knm <- remove_ang(knm)
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
  
  plt <- 
    ggplot(
      data = knm,
      mapping = aes(x=t, y=value, color=side, group=grp)
    ) +
    geom_hline(yintercept = 0, linewidth = .3, color = "grey30") +
    geom_ribbon(
      data = tdavg |> filter(!str_detect(name, "Trk")),
      mapping = aes(ymin=lwr, ymax=upr, color=NA, group=NA),
      fill = "grey80",
      alpha = .4
    ) +
    geom_line(aes(linetype=assdev), linewidth = .3, alpha = .2) + 
    geom_line(
      data = knmavg,
      mapping = aes(group=interaction(side, assdev)),
      linewidth = .6
    ) +
    geom_point(
      data = datlims,
      mapping = aes(group=side),
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
    
    scale_color_discrete_qualitative(palette = "Dark 3", c1 = 80, l1 = 45) + 
    scale_y_continuous(breaks = seq(-90, 90, by = 15)) +
    labs(
      title = glue("{ptname} ({ptmrn}), Exam Date: {ptdate}"),
      x = "Percent Gait Cycle",
      y = ""
    ) +
    guides(color = "none",
           linetype = guide_legend(title = "Assistive Device")) +
    theme_mhs(bs = 6) +
    theme(
      # aspect.ratio = .7,
      axis.text = element_text(size = rel(.75)),
      panel.spacing.x = unit(30, "pt"),
      panel.spacing.y = unit(12, "pt"),
      plot.margin = margin(10, 10, 10, 20),
      legend.position = "bottom"
    )
  
  return(plt)
}