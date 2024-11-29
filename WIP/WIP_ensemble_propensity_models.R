
s <- "Foot_and_Ankle_Soft_Tissue"
prop_vars <- get_prop_model_vars(s)
surg <- glue("interval_{s}")

N = 11

for (kk in 1:N) {
  # Train/Test by Exam_ID -----
  ex <- unique(dat$Exam_ID)
  extrain <- sample(ex, size = round(.7 * length(ex)))
  dtrain <- dat |> filter(Exam_ID %in% extrain)
  dtest <- dat |> filter(!(Exam_ID) %in% extrain)
  
  # Get balanced dataset -----
  ncase <- table(dtrain[[surg]])["1"]
  dtrainbal <- slice_sample(dtrain, n = ncase, by = all_of(surg))
  
  # Make split -----
  xtrain <- dtrain |> select(all_of(prop_vars)) |> data.frame()
  ytrain <- dtrain |> select(matches(surg)) |> pull(1)
  xtrainbal <- dtrainbal |> select(all_of(prop_vars)) |> data.frame()
  ytrainbal <- dtrainbal |> select(matches(surg)) |> pull(1)
  xtest <- dtest |> select(all_of(prop_vars)) |> data.frame()
  ytest <- dtest |> select(matches(surg)) |> pull(1)
  
  # Build model -----
  mod <-
    bartMachine(
      xtrainbal,
      ytrainbal,
      use_missing_data = T,
      serialize = T,
      seed = 42,
      use_missing_data_dummies_as_covars = T
    )
  
  assign(glue("mod_{kk}"), mod)
}


ytest_probx <- list()
ytest_predx <- list()
for (kk in 1:N) {
  cat("Iteration", kk, "\n")
  mod <- get(glue("mod_{kk}"))
  
  ytest_probx[[kk]] <- predict(mod, new_data = xtest)
  ytest_predx[[kk]] <- factor(as.numeric(ytest_probx[[kk]] > mod$prob_rule_class), levels = c("1", "0"))
  # conf <- table(ytest, ytest_pred)
  
}


asdf <- 
  as_tibble(ytest_probx, .name_repair = "unique") %>% 
  mutate(pmean = rowMeans(select(., ...1:...5))) %>% 
  mutate(c_pred = ifelse(pmean >= 0.5, 1, 0)) %>% 
  mutate(c_pred = factor(c_pred, levels = c(1, 0))) %>% 
  mutate(c_meas = ytest) %>% 
  arrange(pmean)

tt <- table(asdf$c_meas, asdf$c_pred)

sn <- tt[1,1]/rowSums(tt)[1]
sp <- tt[2,2]/rowSums(tt)[2]

cat("Specificity = ", round(sp,2), "\nSensitivity = ", round(sn,2))

plot(asdf$pmean, type = "l")
for (kk in 1:N){
  points(asdf[kk], col = kk, pch = 20, cex = .6)
}
lines(asdf$pmean, type = "l", lwd = 2)
abline(h = .5, lwd = 2)
abline(h = c(.4, .6), lty = 2)
abline(v = which(asdf$pmean > .4)[1], lty = 2)
abline(v = which(asdf$pmean > .6)[1], lty = 2)
