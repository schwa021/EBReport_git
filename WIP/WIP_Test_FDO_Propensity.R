temp <- 
  dat %>% 
  filter(
    year(Event_Date) > 2005,
    maxswi_Kne_Ang_Cor > 15,
    meansta_Hip_Ang_Trn > 20
    ) %>% 
  pull(Exam_ID) %>% 
  sample(1)
  
xpt <- 
  dat %>% 
  filter(Exam_ID == temp)

xpt$MRN
xpt$Event_Date
xpt$maxswi_Kne_Ang_Cor


# Add NA for missing features - this is for limbs without gait data -----
vall <- names(datpre)
vx <- names(xpt)
vadd <- vall[!(vall %in% vx)]
xpt[vadd] <- NA

post <- list()
for (s in surglist) {
  mod <- get(glue("mod_{s}"))
  
  set.seed(42)
  xpt[glue("p_{s}")] <- predict(mod, xpt[names(mod$X)])
  post[[s]] <- bart_machine_get_posterior(mod, xpt[names(mod$X)])
}

xL <- xpt |> filter(SIDE == "L")
xR <- xpt |> filter(SIDE == "R")

# Compute propensity as mean of posterior -----
propLmean <- 
  surglist %>% 
  map(\(ss) mean(post[[ss]]$y_hat_posterior_samples[1,])) %>% 
  unlist()

propRmean <- 
  surglist %>% 
  map(\(ss) mean(post[[ss]]$y_hat_posterior_samples[2,])) %>% 
  unlist()

# Compute 5th and 95th and organize. Note "q50" is mean, not median! -----
propL <- 
  surglist %>% 
  map_df(\(ss) quantile(post[[ss]]$y_hat_posterior_samples[1,], probs = c(.05, .95))) %>% 
  mutate(Surgery = str_replace_all(surglist, "_", " ")) %>% 
  mutate(SIDE = "L") %>% 
  rename(q5=`5%`, q95=`95%`) %>% 
  mutate(q50 = propLmean) %>% 
  mutate(
    across(
      starts_with("q"),
      ~ round(100 * .)
    )
  )

propR <- 
  surglist %>% 
  map_df(\(ss) quantile(post[[ss]]$y_hat_posterior_samples[2,], probs = c(.05, .95))) %>% 
  mutate(Surgery = str_replace_all(surglist, "_", " ")) %>% 
  mutate(SIDE = "R") %>% 
  rename(q5=`5%`, q95=`95%`) %>% 
  mutate(q50 = propRmean) %>% 
  mutate(
    across(
      starts_with("q"),
      ~ round(100 * .)
    )
  )

# Generate table -----
proptbl <- ptableLR(propL, propR)
proptbl

# Loop over surgeries -----
tall <- list()

# Build detail tables -----
ss <- "Femoral_Derotation_Osteotomy"
for (s in ss) {
  mod <- get(glue("mod_{s}"))
  res <- prop_shap_table(df=dat, sname=s, mod, x=xpt, varlabs)
  tall[[s]] <- res
}

ptprops <- bind_rows(propL, propR)

# Format detail tables -----
tLR <- list()
for (s in ss) {
  tLR[[s]] <- fmt_shapLR(tall, ptprops, s)
}

tLR



