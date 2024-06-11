
lwr <- -20
upr <- 20
nout <- 100

asdf <- as_tibble(t(rbind(post0, post1)))

cdf_fun <- function(y, lwr, upr){
  cdf <- seq(lwr, upr, length.out = nout) %>% map_dbl(\(x) round(100 * sum(y < x) / 1000))
}

xxx <- c("V1", "V2", "V3", "V4") %>% map(\(y) cdf_fun(asdf[[y]], lwr, upr) )

res <- tibble(
  x = seq(lwr, upr, length.out = nout),
  L_control = xxx[[1]],
  R_control = xxx[[2]],
  L_treated = xxx[[3]],
  R_treated = xxx[[4]]
) %>% 
  pivot_longer(-x, names_sep = "_", names_to = c("side", "status"))


ggplot(res, aes(x=x, y=value, color=interaction(status, side))) + 
  geom_line(linewidth = .5) + 
  geom_vline(xintercept = -5, color="grey60", linetype = "dashed") +
  facet_wrap(~side) + 
  labs(
    title = str_replace_all(s, "_", " "),
    x = glue("Change in {vlabs[vv]}"),
    y = "Probability of Exceeding Change"
  ) +
  scale_color_manual(values = c("red4", "red1", "green4", "green1"), name = "") +
  theme(
    legend.position = "bottom"
  )


