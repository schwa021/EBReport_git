tit <- glue("{ptname} ({params$MRN}), Exam Date: {ptdate}")

knm <- 
  datc3d$Ang |> 
  list_rbind() |> 
  mutate(grp = interaction(cycle, side, Trial_Num))

knmavg <- 
  datc3d$Ang_avg |>
  list_rbind()

datlwr <-
  tibble(
    name = unique(knmavg$name),
    value = c(c(-15, -15, -15), c(-15, -15, -15), c(-15, -15, -30), c(-15, -30, -45), c(-30, -75, -60))
  )
datupr <-
  tibble(
    name = unique(knmavg$name),
    value = c(c(30, 60, 30), c(30, 75, 30), c(15, 75, 15), c(15, 30, 30), c(15, 15, 30))
  )
datlims <- 
  bind_rows(datlwr, datupr) |> 
  mutate(
    t = as.numeric(NA),
    side = as.character(NA)
    )


ggplot(
  data = knm,
  mapping = aes(x=t, y=value, color=side, group=grp)
) +
  geom_line(linewidth = .1, alpha = .2) + 
  geom_line(
    data = knmavg,
    mapping = aes(x=t, y=value, color=side, group=side),
    linewidth = 1
  ) +
  geom_point(
    data = datlims,
    mapping = aes(x=t, y=value, color=side, group=side),
  ) +
  facet_wrap(~ name, ncol = 3, scales = "free_y") +
  scale_color_discrete_qualitative(palette = "Dark 3") + 
  scale_y_continuous(breaks = seq(-90, 90, by = 15)) +
  labs(
    title = glue("{ptname} ({params$MRN}), Exam Date: {ptdate}"),
    x = "Percent Gait Cycle",
    y = "Value"
  ) +
  guides(color = "none") +
  theme_mhs(bs = 8)
