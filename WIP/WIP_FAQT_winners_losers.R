# Function to split data -----
fsplit <- function(dat, ix, pred_vars, outvar){
  outvarPost <- glue("{outvar}Post")
  dout <- dat %>% filter(ix) %>% data.frame() %>% drop_na(all_of(c(outvar, outvarPost)))
  xout <- dout %>% dplyr::select(all_of(pred_vars))
  yout <- dout %>% 
    mutate(
      del = .data[[outvarPost]] - .data[[outvar]]
    ) %>% 
    pull(del)
  return(list("d" = dout, "x" = xout, "y" = yout))
}

# Get features -----
predvars <- get_pred_model_vars(vv)
predvars <- c("age", "GMFCS", predvars, glue("interval_{surglist}"))
predvars <- unique(predvars)

# Split into training and testing -----
set.seed(42)
rnum <- runif(nrow(dat))
itrain <- between(rnum, 0, .8)
itest <- between(rnum, .8, 1)

# Make split -----
dtrain <- fsplit(dat, itrain, predvars, vv)$d
xtrain <- fsplit(dat, itrain, predvars, vv)$x
ytrain <- fsplit(dat, itrain, predvars, vv)$y

dtest <- fsplit(dat, itest, predvars, vv)$d
xtest <- fsplit(dat, itest, predvars, vv)$x
ytest <- fsplit(dat, itest, predvars, vv)$y

# Build control x
xtest0 <- 
  xtest %>% 
  mutate(
    across(
      glue("interval_{surglist}"),
      ~ factor("0", levels = levels(dat$interval_Adductor_Release))
    )
  )

# Build model -----
mod <- bartMachine(xtrain, ytrain, use_missing_data = T, serialize = T, seed = 42)

# Test model -----
temp <- calc_prediction_intervals(mod, xtest, pi_conf = .90)
testint <- temp$interval
testval <- rowMeans(temp$all_prediction_samples)

temp0 <- calc_prediction_intervals(mod, xtest0, pi_conf = .90)
testint0 <- temp0$interval
testval0 <- rowMeans(temp0$all_prediction_samples)

# Get details
deets <- fsplit(dat, itest, c("Exam_ID", "SIDE"), vv)$x

testperf <- tibble(
  Exam_ID = deets$Exam_ID,
  SIDE = temp$SIDE,
  meas = ytest,
  pred = testval,
  pred0 = testval0
)

temp <- 
  inner_join(dat, testperf) %>% 
  filter(abs(pred) >= 7.5) %>% 
  select(MRN, Event_Date, SIDE, age, GMFCS, pred, pred0, FAQT, FAQTPost, all_of(glue("interval_{surglist}")))

ggplot(asdf$testperf, aes(x = pred, y = meas, color = cover)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", aes(group = 1), color = "grey10") +
  labs(x = "Predicted Change", y = "Measured Change", title = "FAQT") +
  scale_color_discrete_diverging(rev = TRUE)
