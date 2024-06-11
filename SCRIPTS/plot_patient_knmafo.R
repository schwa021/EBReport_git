plot_patient_knmafo <- function(datc3d, datc3d_afo, xpt, sd, ptname, ptdate, ptmrn) {
  
  # ptname <- glue("{xpt$Child_First} {xpt$Child_Last}")[1]
  # ptdate <- format(as.Date(params$Event_Date), "%b %d, %Y")
  # ptmrn <- xpt$MRN[1]
  tit <- glue("{ptname} ({ptmrn}), Exam Date: {ptdate}")
  
  knm <- 
    datc3d$Ang |> 
    list_rbind() |> 
    filter(
      Exam_ID == xpt$Exam_ID[1],
      side == sd,
    ) |>
    mutate(orthdev = ifelse(orthdev=="None", "Barefoot", orthdev)) |> 
    mutate(grp = interaction(cycle, side, Trial_Num))
  
  knmafo <- 
    datc3d_afo$Ang |> 
    list_rbind() |> 
    filter(
      Exam_ID == xpt$Exam_ID[1],
      side == sd
    ) |>
    mutate(orthdev = ifelse(orthdev=="None", "Barefoot", orthdev)) |> 
    mutate(grp = interaction(cycle, side, Trial_Num))
  
  knmx <- 
    bind_rows(knm, knmafo) |> 
    mutate(cond = glue("ASST: {assdev}, ORTH: {orthdev}"))
  
  # Get levels for condition -----
  asstnone <- str_subset(unique(knmx$cond), "None")
  asstasst <- str_subset(unique(knmx$cond), "None", negate = T)
  condlevs <- c(asstnone, asstasst)
  temp <- str_subset(unique(knmx$orthdev), "Barefoot", negate = T)
  afolevs <- c("Barefoot", temp)
  temp <- str_subset(unique(knmx$assdev), "None", negate = T)
  asslevs <- c("None", temp)
  
  knmx <- 
    knmx |> 
    mutate(cond = factor(cond, levels = condlevs))
  
  knmavg <- 
    datc3d$Ang_avg |>
    list_rbind() |> 
    filter(
      Exam_ID == xpt$Exam_ID[1], 
      side == sd
    ) |> 
    mutate(orthdev = ifelse(orthdev=="None", "Barefoot", orthdev))
  
  knmavgafo <- 
    datc3d_afo$Ang_avg |>
    list_rbind() |> 
    filter(
      Exam_ID == xpt$Exam_ID[1],
      side == sd
    ) |> 
    mutate(orthdev = ifelse(orthdev=="None", "Barefoot", orthdev))
  
  knmavgx <- 
    bind_rows(knmavg, knmavgafo) |> 
    mutate(cond = glue("ASST: {assdev}, ORTH: {orthdev}")) |> 
    mutate(cond = factor(cond, levels = condlevs))
  
  # Set afo levels -----
  # knmx <- 
  #   knmx |> 
  #   mutate(orthdev = factor(orthdev, levels = afolevs)) |> 
  #   mutate(assdev = factor(assdev, levels = asslevs))
  # 
  # knmavgx <- 
  #   knmavgx |> 
  #   mutate(orthdev = factor(orthdev, levels = afolevs)) |> 
  #   mutate(assdev = factor(assdev, levels = asslevs))
  
  
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
  knmx <- remove_ang(knmx)
  knmavgx <- remove_ang(knmavgx)
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
  # Color scheme -----
  ncond <- length(unique(knmx$cond))
  
  if(ncond == 2){
    hue <- ifelse(sd == "L", 346, 173)
    clr <- 
      sequential_hcl(
        2,
        h = hue,
        c = 50,
        l = c(55, 20),
        power = 1.3
      )
    
    clr[2] <- "#6B7FCF"
    clr <- rev(clr)
  } else {
    clr <- qualitative_hcl(ncond, palette = "Dark 3")
  }
  
  plt <- 
    ggplot(
      data = knmx,
      mapping = aes(x=t, y=value, group=grp)
    ) +
    
    ggh4x::facet_manual(~name, design = design, scales = "free_y") +
    
    geom_hline(yintercept = 0, linewidth = .3, color = "grey30") +
    
    geom_ribbon(
      data = tdavg |> filter(!str_detect(name, "Trk")),
      mapping = aes(ymin=lwr, ymax=upr, group=NA),
      fill = "grey80",
      color = "transparent",
      alpha = .4
    ) +
    
    geom_line(aes(color = cond), linewidth = .2, alpha = .2) +
    
    geom_line(
      data = knmavgx,
      mapping = aes(group=cond, color = cond),
      linewidth = .6
    ) +
    
    geom_point(
      data = datlims,
      mapping = aes(x=t, y=value, group=side),
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
    
    scale_color_manual(values = clr, name = "Condition") +
    scale_y_continuous(breaks = seq(-90, 90, by = 15)) +
    labs(
      title = glue("{ptname} ({ptmrn}), Exam Date: {ptdate}"),
      x = "Percent Gait Cycle",
      y = ""
    ) +
    theme_mhs(bs = 6) +
    theme(
      axis.text = element_text(size = rel(.75)),
      panel.spacing.x = unit(30, "pt"),
      panel.spacing.y = unit(12, "pt"),
      plot.margin = margin(10, 10, 10, 20),
      legend.position = "bottom"
    )
  
  return(plt)
}
