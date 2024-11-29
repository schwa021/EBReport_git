
source("SCRIPTS/pd_plot_mhs.R")
source("SCRIPTS/pd_BM.R")
mod_Neural_Rhizotomy <- mod

pd_s <- pd_plot_mhs(s, prop = .1)
pout <- patchwork::wrap_plots(pd_s, ncol = 4)

p_pd <-
  pout &
  theme(
    plot.margin = unit(c(8,2,8,2), "pt")
  ) & 
  annotate(
    geom = "text",
    x = -Inf,
    y = c(Inf, -Inf),
    label = c("High Probability", "Low Probability"),
    hjust = c(0,0),
    vjust = c(1,-1),
    color = c("#005600", "#841859"),
    size = rel(1)
  )

p_pd
