
# This function calculates several diagnostic metrics for a given threshold -----
calculate_metrics <- function(threshold, data, cost) {
  predicted_positive <- data$probabilities >= threshold
  actual_positive <- data$actual_classes == 1
  prev <- sum(data$actual_classes==1)/nrow(data)
  
  TP <- sum(predicted_positive & actual_positive) |> as.double()
  FP <- sum(predicted_positive & !actual_positive) |> as.double()
  TN <- sum(!predicted_positive & !actual_positive) |> as.double()
  FN <- sum(!predicted_positive & actual_positive) |> as.double()
  
  sens <- TP / (TP + FN)
  spec <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  return(c(thresh = threshold, acc = (TP + TN) / (nrow(data)), TP = TP, FP = FP,
           TN = TN, FN = FN, ppv = ppv, npv = npv, sens = sens, spec = spec,
           J = sens + spec - 1, d2 = (1 - sens)^2 + ((1-prev)/(cost*prev))*(1 - spec)^2,
           MCC = MCC, FPminusFN = cost * FP - FN))
}

s <- "Femoral_Derotation_Osteotomy"
surg <- glue("interval_{s}")
prop_vars <- c(get_prop_model_vars(s), "rnd")
prop_vars <- c("age", "FemTor", "meansta_Hip_Ang_Trn", "meansta_Foo_Ang_Trn", "Sex")
fp2fn=1
df <- 
  dat %>% 
  mutate(
    GMFCS = case_when(
      dx != "Cerebral palsy" ~ "Missing",
      TRUE ~ as.character(GMFCS)
    ),
    rnd = runif(nrow(dat))
  ) %>%
  mutate(GMFCS = factor(GMFCS, levels = c("I", "II", "III", "IV", "V", "Missing")))


# Define training and testing sets -----
set.seed(42)
ex <- unique(df$Exam_ID)
extrain <- sample(ex, size = round(.7 * length(ex)))
dtrain <- df |> filter(Exam_ID %in% extrain)
dtest <- df |> filter(!(Exam_ID) %in% extrain)

# Get balanced dataset for training -----
ncase <- table(dtrain[[surg]])["1"]
dtrainbal <- slice_sample(dtrain, n = ncase, by = all_of(surg))

# Make data split -----
xtrain <- dtrain |> select(all_of(prop_vars)) |> data.frame()
ytrain <- dtrain |> select(matches(surg)) |> pull(1)
xtrainbal <- dtrainbal |> select(all_of(prop_vars)) |> data.frame()
ytrainbal <- dtrainbal |> select(matches(surg)) |> pull(1)
xtest <- dtest |> select(all_of(prop_vars)) |> data.frame()
ytest <- dtest |> select(matches(surg)) |> pull(1)

# CART -----
library(rpart)
library(rpart.plot)

dd <- 
  dtrainbal %>% 
  # select(age, dx, all_of(glue("prior_{s}")), y=all_of(glue("interval_{s}")))
  select(all_of(prop_vars), y=all_of(glue("interval_{s}"))) %>% 
  mutate(y = factor(y, levels = c("0", "1")))

cart <- rpart(y ~ ., data = dd, method = "class", cp = .02)
rpart.plot(cart, type = 2)
cart_pred <- predict(cart, xtest, type = "class")
table(ytest, cart_pred)

# Get dummified data organization -----
temp <-
  bartMachine(
    bind_cols(xtrainbal, cart=cart_pred),
    ytrainbal,
    num_trees = 10,
    num_burn_in = 10,
    num_iterations_after_burn_in = 10,
    use_missing_data = T,
    serialize = T,
    seed = 42,
    use_missing_data_dummies_as_covars = T,
  )
cov_prior_names <- temp$training_data_features_with_missing_features

# Get original model and performance -----
mod_0 <-
  bartMachine(
    xtrainbal,
    ytrainbal,
    use_missing_data = T,
    serialize = T,
    seed = 42,
    use_missing_data_dummies_as_covars = T,
  )

# Get original model and performance -----
mod_cart <-
  bartMachine(
    bind_cols(xtrainbal, cart=cart_pred),
    ytrainbal,
    use_missing_data = T,
    serialize = T,
    seed = 42,
    use_missing_data_dummies_as_covars = T,
  )


# TEST CODE NEW FEATURE -----
# df <- df |>
#   mutate(
#     dxcp = case_when(
#       dx == "Cerebral palsy" ~ "Cerebral palsy",
#       TRUE ~ "Other"
#     )
#   ) %>%
#   mutate(dxcp = factor(dxcp, levels = c("Cerebral palsy", "Other")))
# prop_vars <- c(prop_vars, "dxcp")
# prop_vars <- prop_vars[prop_vars != "dx"]


# TEST CODE EXAMINING COVARIATE WEIGTING -----
cov_prior_names <- mod_0$training_data_features_with_missing_features

# SDR
ix_dxcp <- which(str_detect(cov_prior_names, "Cerebral palsy$"))
ix_priorsdr <- which(str_detect(cov_prior_names, "^prior_Neural_Rhizotomy_1"))
cov_prior_vec <- rep(1, length(cov_prior_names))
cov_prior_vec[ix_dxcp] <- 10
cov_prior_vec[ix_priorsdr] <- 10
names(cov_prior_vec) <- cov_prior_names

# FDO
# ix_femtor <- which(str_detect(cov_prior_names, "^FemTor"))
# cov_prior_vec <- rep(1, length(cov_prior_names))
# cov_prior_vec[ix_femtor] <- 100

# Train model -----
mod_wt <-
  bartMachine(
    xtrainbal,
    ytrainbal,
    use_missing_data = T,
    serialize = T,
    seed = 42,
    use_missing_data_dummies_as_covars = T,
    cov_prior_vec = cov_prior_vec
  )

mod_test <- mod_wt

# Train model no dx-----
mod_x <-
  bartMachine(
    xtrainbal,
    ytrainbal,
    use_missing_data = T,
    serialize = T,
    seed = 42,
    use_missing_data_dummies_as_covars = T,
    # cov_prior_vec = cov_prior_vec
  )


testmod <- function(mod_test, xtest, ytest){
  # Evaluate model on test data -----
  ytest_prob <- predict(mod_test, new_data = xtest[names(mod_test$X)])
  ytest_pred <- factor(as.numeric(ytest_prob > mod_test$prob_rule_class), levels = c("1", "0"))
  conf <- table(ytest, ytest_pred)
  
  # Generate metrics for test data -----
  data <- data.frame(probabilities = ytest_prob, actual_classes = ytest)
  prev <- sum(ytrain==1)/length(ytrain)
  wt_fp2fn <- 1
  metrics <-
    map(.x = round(seq(0, 1, .01), 2), .f = calculate_metrics, data = data, cost = 1/wt_fp2fn) |>
    bind_rows()
  
  # AUC -----
  f <- splinefun(x = 1 - metrics$spec, y = metrics$sens, ties = mean)
  auc <- integrate(f, 0, 1)$value |> round(2)
  
  # Organize optimal test metrics -----
  opttest <- data.frame(
    prevalence = prev,
    sensitivity = conf["1", "1"] / (conf["1", "1"] + conf["1", "0"]),
    specificity = conf["0", "0"] / (conf["0", "1"] + conf["0", "0"]),
    accuracy = (conf["1", "1"] + conf["0", "0"])/sum(conf),
    ppv = conf["1", "1"] / (conf["1", "1"] + conf["0", "1"]),
    npv = conf["0", "0"] / (conf["1", "0"] + conf["0", "0"]),
    auc = auc
  ) |> 
    round(2)
  
  return(opttest)
}

