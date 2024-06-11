# fname <-
#   "C:/Users/mschwartz/OneDrive - Gillette Children's Specialty Healthcare/Read C3D/ReadAll/OUTPUT/data_avg_current.RData"
#
# load(fname)
# angavg <- list_rbind(Ang_avg)

ppta <-
  match_kinematics(xL, "L", "DFEO", "Control", angavg, dat)
ggsave("junkc.png", width = 7, height = 7, dpi=600, bg="white")

ppta <-
  match_kinematics(xL, "L", "DFEO", "Treated", angavg, dat)
ggsave("junkt.png", width = 7, height = 7, dpi=600, bg="white")

match_kinematics <- function(xx, sd, s, torc, angavg, dat) {
  # Build plot annotation -----
  tit <- str_replace_all(s, "_", " ")
  
  # Get matched data for surgery s -----
  dmatch <- pmatch_data(s, datx = dat, xside = xx)
  m <- if (torc == "Control") {
    m <- dmatch$dmatch0
  } else {
    m <- dmatch$dmatch1
  }
  
  # get kinematics of matches -----
  gm <-
    angavg |>
    filter(Exam_ID %in% m$Exam_ID)
  
  gmavg <- 
    gm |> 
    group_by(t, name) |> 
    summarize(value = mean(value, na.rm = T))  
  
  gmPost <- 
    angavg |> 
    filter(Exam_ID %in% m$Exam_IDPost)
  
  gmavgPost <- 
    gmPost |> 
    group_by(t, name) |> 
    summarize(value = mean(value, na.rm = T))
  
  # Get average kinematics of patient side -----
  gptavg <-
    datc3d$Ang_avg |>
    list_rbind() |>
    filter(Exam_ID == xpt$Exam_ID[1],
           side == "L")
  
  # Get data for setting plot limits -----
  datlwr <-
    tibble(name = unique(gptavg$name),
           value = c(
             c(-15,-15,-15),
             c(-15,-15,-15),
             c(-15,-15,-30),
             c(-15,-30,-45),
             c(-30,-75,-60)
           ))
  datupr <-
    tibble(name = unique(gptavg$name),
           value = c(
             c(30, 60, 30),
             c(30, 75, 30),
             c(15, 75, 15),
             c(15, 30, 30),
             c(15, 15, 30)
           ))
  datlims <-
    bind_rows(datlwr, datupr) |>
    mutate(t = as.numeric(NA),
           side = as.character(NA))
  
  ptcol <- ifelse(sd == "L", "#c54e6d", "#009380")
  
  p <-
    ggplot(mapping = aes(x = t, y = value)) +
    # geom_line(
    #   data = gm,
    #   mapping = aes(group = interaction(Exam_ID, side)),
    #   linewidth = .1,
    #   alpha = .3,
    #   color = "grey50"
    # ) +
    geom_line(data = gmavg,
              linewidth = .8,
              color = "grey30") +
    # geom_line(
    #   data = gmPost,
    #   mapping = aes(group = interaction(Exam_ID, side)),
    #   linewidth = .1,
    #   alpha = .3,
    #   color = "lightblue"
    # ) +
    geom_line(data = gmavgPost,
              linewidth = .8,
              linetype = "dashed",
              color = "grey30") +
    geom_line(data = gptavg,
              color = ptcol,
              linewidth = .8) +
    geom_point(data = datlims,
               mapping = aes(x = t, y = value), ) +
    facet_wrap(~ name, ncol = 3, scales = "free_y") +
    labs(title = str_replace_all(s, "_", " "),
         subtitle = glue("{sd}-side, {torc}"))
  
  return(p)
}
