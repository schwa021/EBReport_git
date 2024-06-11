sx <- "Psoas_Release"
outcome <- "minsta_Hip_Ang_Sag"

px <- dat[[glue("p_{sx}")]]
rx <- dat[[glue("interval_{sx}")]]

y <- dat[[glue("{outcome}Post")]] - dat[[glue("{outcome}")]]

d <- tibble(
  propensity = px,
  treatment = rx,
  outcome = y
)

d <-
  d %>% 
  mutate(pcat = cut(propensity, c(-99, .1, .9, 99), labels = c("low", "med", "high"))) %>%
  mutate(grp = interaction(treatment, pcat)) %>% 
  mutate(grp = factor(grp, labels = c("NoSDR, Low", "YesSDR, Low", "NoSDR, Med", "YesSDR, Med", "NoSDR, High", "YesSDR, High"))) %>% 
  mutate(treatment = factor(treatment, labels = c(glue("No {sx}"), glue("Yes {sx}"))))


ggplot(d, aes(x=propensity, y=outcome, color=pcat, fill=pcat)) + 
  geom_hline(yintercept = 0, size = .2, color = "grey30") +
  geom_jitter(size = .2, alpha = .1) +
  geom_smooth(method = "lm", formula=y ~ 1) +
  facet_wrap(~ treatment) +
  coord_cartesian(ylim = c(-10, 10), clip = "off") +
  ylab(glue("Change in {outcome}")) +
  xlab("Propensity") +
  labs(title = glue("{sx}")) +
  guides(color = "none", fill = "none") +
  theme(
    strip.text = element_text(size = 6, face = "bold")
  )

ggplot(d, aes(x=pcat, y=outcome, color=pcat, fill=pcat)) + 
  geom_hline(yintercept = 0, size = .2, color = "grey30") +
  geom_jitter(size = .2, alpha = .2, width = .25) +
  geom_boxplot(notch = T, alpha = .5, outlier.colour = NA) +
  facet_wrap(~ treatment) +
  coord_cartesian(ylim = c(-30, 30), clip = "off") +
  ylab(glue("Change in {outcome}")) +
  xlab("Propensity") +
  labs(title = glue("{sx}")) +
  guides(color = "none", fill = "none") +
  theme(
    strip.text = element_text(size = 6, face = "bold"),
    plot.title = element_text(margin = margin(b=25))
  )


    
ggsave("OUTPUT/junk.png", width = 6, height = 4, dpi = 300, bg = "white")       
