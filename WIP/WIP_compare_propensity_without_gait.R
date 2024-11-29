
# Load models with gait data -----
for (s in surglist) {
    load(glue("OUTPUT/Propensity_Models/prop_{s}.RDATA"))
    assign(glue("mod_{s}"), slist[[1]])
    assign(glue("opt_{s}"), slist[[2]])
}

# Build models without gait data -----
for (s in surglist) {
    # Set usegait to build with (TRUE) or without (FALSE) gait data -----
    usegait = FALSE
    surg <- glue("interval_{s}")
    res <- build_propensity_mod(s, dat, fp2fn=1, usegait)
    assign(glue("res_{s}"), res)
}

# Compute nogait propensities -----
if(is.null(dat$p_Femoral_Derotation_Osteotomy_nogait)){
  for (s in surglist) {
    cat("Computing Propensity for Surgery ", s, "\n")
    res <- get(glue("res_{s}"))
    mod <- res$mod
    set.seed(42)
    dat[[glue("p_{s}_nogait")]] <- predict(mod, dat[names(mod$X)])
  }
}


# Build gait vs. nogait propensity plots -----
plist <- list()

for (s in surglist) {
  v0 <- glue("p_{s}_nogait")
  v1 <- glue("p_{s}")
  plist[[s]] <- 
    ggplot(dat, aes(x=!!sym(v0), y=!!sym(v1))) + 
    geom_point(size = .1) + 
    geom_abline(intercept=0, slope=1, color="blue") +
    geom_smooth(method = "lm") +
    coord_fixed(xlim=c(0, 1), ylim=c(0, 1)) +
    labs(
      x= "No Gait Data",
      y= "Yes Gait Data",
      title=glue("{s}"),
    ) +
    theme_mhs(5)
}

# wrap_plots(plist)

ggsave("junk.png", plot=wrap_plots(plist), width=8, height=8, dpi=600, bg="white")
