# Data from Gaumetou, 2014 - EOS analysis of lower extremity segmental torsion in children and young adults

ft <- c(21.6, 19.4, 19, 15.3, 18)
ftsig <- c(10, 6.4, 9, 8, 9.1)

tt <- c(26.8, 25.9, 29., 32.8, 34.7)
ttsig <- c(6.6, 6.7, 6.3, 8.3, 7.8)

age <- c(6.6, 8.9, 10.9, 14.1, 23.3)

ftl <- approx(age, ft, xout = c(6, 9, 12, 15, 18), rule = 2)
ttl <- approx(age, tt, xout = c(6, 9, 12, 15, 18), rule = 2)


fts <- spline(age, ft, xout = c(6, 9, 12, 15, 18), method = "natural")
ftsigs <- spline(age, ftsig, xout = c(6, 9, 12, 15, 18), method = "natural")

tts <- spline(age, tt, xout = c(6, 9, 12, 15, 18), method = "natural")
ttsigs <- spline(age, ttsig, xout = c(6, 9, 12, 15, 18), method = "natural")


plot(fts$x, fts$y, xlim = c(0,25), ylim = c(0,25), type = "l")
lines(ftl$x, ftl$y, col = "blue")
points(age, ft, col = "red")

plot(tts$x, tts$y, xlim = c(0,25), ylim = c(20,50), type = "l")
lines(ttl$x, ttl$y, col = "blue")
points(age, tt, col = "red")


ftlwr <- fts$y - 1.645*ftsigs$y
ftupr <- fts$y + 1.645*ftsigs$y
ttlwr <- tts$y - 1.645*ttsigs$y
ttupr <- tts$y + 1.645*ttsigs$y

ftdat <- tibble(lwr=ftlwr, value=fts$y, upr=ftupr)
ttdat <- tibble(lwr=ttlwr, value=tts$y, upr=ttupr)


# Data from Carman L (Besier) 2023 -----

# Tibial
tt <- 
  readxl::read_xlsx("DATA/Tibfib_clinical_bone_measurements.xlsx", .name_repair = "universal")

names(tt) <- 
  names(tt) |> 
  str_replace_all("_", ".") |> 
  str_replace_all("\\.\\.", "\\.") |> 
  str_remove_all(".cm.|.Degrees.|.years.|\\.")

ttdatx <- 
  tt |> 
  mutate(age = cut(Age, seq(1.5, 19.5, 3), labels = c(3, 6, 9, 12, 15, 18))) |> 
  group_by(age) |> 
  summarize(
    lwr = quantile(Tibialtorsion, probs = .05, na.rm = T),
    value = mean(Tibialtorsion, na.rm = T),
    upr = quantile(Tibialtorsion, probs = .95, na.rm = T)
  )


ggplot(tt, aes(x=Age, y=Tibialtorsion)) +
  geom_jitter(color = "red", size = .5) +
  # geom_smooth(span = 1, color = "red", se = F) +
  geom_pointrange(
    data = ttdatx,
    aes(x=as.numeric(as.character(age)), y=value, ymin=lwr, ymax=upr),
    color = "red",
    linewidth = 1,
    size = .8
  ) +
  geom_pointrange(
    data = ttdat,
    aes(x=as.numeric(as.character(age)), y=value, ymin=lwr, ymax=upr),
    color = "blue",
    linewidth = 1,
    size = .8    
  )
scale_x_continuous(breaks = seq(0,18,3))


ft <- 
  readxl::read_xlsx("DATA/Femur_clinical_bone_measurements.xlsx", .name_repair = "universal")

names(ft) <- 
  names(tt) |> 
  str_replace_all("_", "")

ftdatx <- 
  ft |> 
  mutate(age = cut(Age, seq(1.5, 19.5, 3), labels = c(3, 6, 9, 12, 15, 18))) |> 
  group_by(age, Sex) |> 
  summarize(
    lwr = quantile(AA2d, probs = .05, na.rm = T),
    value = mean(AA2d, na.rm = T),
    upr = quantile(AA2d, probs = .95, na.rm = T)
  )


ggplot(ft, aes(x=Age, y=AA2d)) +
  geom_jitter(aes(color = Sex), size = .5) +
  geom_pointrange(
    data = ftdatx,
    aes(x=as.numeric(as.character(age)), y=value, ymin=lwr, ymax=upr, color=Sex),
    linewidth = 1,
    size = .8,
    position = position_dodge(width=.5)
  ) +
  geom_pointrange(
    data = ftdat,
    aes(x=as.numeric(as.character(age)), y=value, ymin=lwr, ymax=upr),
    color = "green4",
    linewidth = 1,
    size = .8    
  ) +
  scale_x_continuous(breaks = seq(0,24,3), limits = c(0,24)) +
  scale_color_discrete_diverging("Blue-Red 2", rev = T)

