# ++++++++++++++++++++++++++++++++++++++++++++++ #
# File containing functions for comparing models #
# ++++++++++++++++++++++++++++++++++++++++++++++ #

# ---- Function to build propensity model using CART ----
build_prop_CART <- 
  function(df, prop_list_BART, s){
    # Define interval surgery name -----
    interval <- glue("interval_{s}")
    
    # Suppress warnings ----
    options(warn = -1)
    
    # Get data -----
    dat_train <- suppressMessages(inner_join(df, prop_list_BART[[s]]$X))
    dat_test <- setdiff(df, dat_train)
    vlist <- names(prop_list_BART[[s]]$X)
    
    dat_train <- dat_train %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    nsamp <- nrow(dat_train)
    
    dat_test <- dat_test %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    
    # Define classifier and workflow -----
    tree_spec <- 
      decision_tree(
        cost_complexity = tune(),  # `cp` in rpart
        tree_depth = tune(),       # `maxdepth` in rpart
        min_n = tune()             # `minsplit` in rpart
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")  # Use "regression" for numeric outcomes
    
    # Get data for cross-validation folds -----
    Ns <- sum(dat_train[[interval]]=="1")
    dat_bal <- slice_sample(dat_train, n = Ns, by = all_of(interval))
    
    dat_cv <-
      dat_bal %>%
      select(all_of(vlist), y)
    cv_folds <- vfold_cv(dat_cv, v = 10) 
    
    # Build workflow -----
    tree_wf <- 
      workflow() %>%
      add_model(tree_spec) %>%
      add_formula(y ~ .)
    
    # Grid Search: Random search is fast and works with flat cost -----
    set.seed(42)
    tree_grid <- 
      grid_random(
        cost_complexity(range = c(-4, 0)),
        tree_depth(range = c(2, 4)),
        min_n(range = c(round(nsamp/20), round(nsamp/10))),
        size = 50
      )
    
    tree_results <- 
      tune_grid(
        tree_wf,
        resamples = cv_folds,
        grid = tree_grid,
        metrics = metric_set(sens, spec, bal_accuracy, ppv, npv, f_meas)
      )
    
    # Choose optimal parameters -----
    mopt <- "bal_accuracy"
    best_params <- select_best(tree_results, metric = mopt)
    final_tree <- finalize_model(tree_spec, best_params)
    final_wf <- 
      workflow() %>%
      add_model(final_tree) %>%
      add_formula(y ~ .)
    
    # Train final model ----
    final_fit <- fit(final_wf, data = dat_cv)
    mod <- final_fit %>% extract_fit_engine()
    
    # Plot final model -----
    rpart.plot(mod, type = 5, extra = 104, varlen = 14, faclen=9, cex = .6, roundint = F)
    
    # Evaluate the model ----
    y_pred <- predict(mod, newdata = dat_test, type = "class")
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,1]
    dat_test$p_CART <- y_prob
    pred <- tibble(truth=dat_test$y, pred_class=y_pred, class1_prob=y_prob)
    
    # Compute sens, spec, ppv, npv, accuracy using yardstick metrics ----
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=truth, estimate=pred_class)
    perf$surgery <- s
    perf$.estimate <- round(perf$.estimate, 2)
    
    # Return warnings and result ----
    options(warn = 0)
    
    return(list(mod=mod,perf=perf,dat_train=dat_train, dat_test=dat_test))
  }

# ---- Function to build propensity model using Naive Bayes ----
build_prop_NB <- 
  function(df, prop_list_BART, s){
    # Suppress warnings ----
    options(warn = -1)
    
    # Get data -----
    dat_train <- suppressMessages(inner_join(df, prop_list_BART[[s]]$X))
    dat_test <- setdiff(df, dat_train)
    vlist <- names(prop_list_BART[[s]]$X)
    
    dat_train <- dat_train %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    dat_test <- dat_test %>% 
      mutate(
        y = .data[[glue("interval_{s}")]],
        p_BART = .data[[glue("p_{s}")]]
      )
    
    # Build the Naive Bayes model -----
    mod <- 
      naive_bayes(
        y ~ .,
        data = dat_train %>% select(all_of(vlist), y),
        usekernel = TRUE, usepoisson = FALSE, laplace = 1
      )
    
    # Evaluate the model ----
    y_pred <- predict(mod, newdata = dat_test)
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,1]
    dat_test$p_Naive <- y_prob
    pred <- tibble(truth=dat_test$y, pred_class=y_pred, class1_prob=y_prob)
    
    # Compute sens, spec, ppv, npv, accuracy using yardstick metrics ----
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy)
    perf <- mlist(pred, truth=truth, estimate=pred_class)
    perf$surgery <- s
    perf$.estimate <- round(perf$.estimate, 2)
    
    # Return warnings and result ----
    options(warn = 0)
    
    return(list(mod=mod,perf=perf,dat_train=dat_train, dat_test=dat_test))
  }

# ---- Get propensity table for patient ----
build_ptprop <- 
  function(ypt_prob){
    ptprops <- 
      as_tibble(ypt_prob) %>% 
      mutate(SIDE = c("Left", "Right")) %>% 
      pivot_longer(
        cols = -SIDE,
        names_to = "Surgery",
        values_to = "q50"
      ) %>% 
      mutate(
        Surgery = vlabs[Surgery],
        q5 = NA, q25 = NA, q75 = NA, q95 = NA, 
        q50 = round(100 * q50)
      ) %>% 
      relocate(q5, q25, q75, q95, Surgery, SIDE, q50) %>% 
      mutate(Surgery = factor(Surgery, levels = c("Femoral Derotation Osteotomy", "Tibial Derotation Osteotomy", "Psoas Lengthening", "Hamstring Lengthening", "Adductor Lengthening", "Calf Muscle Lengthening", "DFEO + Patellar Advance", "Patellar Advance", "Foot and Ankle Bone", "Foot and Ankle Soft Tissue", "Selective Dorsal Rhizotomy", "Rectus Femoris Transfer"))) %>% 
      arrange(SIDE, Surgery)
    
    return(ptprops)
  }

# ---- Format propensity table for patient ----
format_proptable_pt <- function(ptprops){
  datL <- ptprops %>% filter(SIDE == "Left") %>% mutate(SIDE = "L")
  datR <- ptprops %>% filter(SIDE == "Right") %>% mutate(SIDE = "R")
  proptable <- ptableLR(datL, datR)
  return(proptable)
}

# ---- Function to compare BART vs. NB vs. CART ----
perf_compare_NB_CART_BART <- 
  function(NBlist, CARTlist, BARTlist, s){
    perf_NB <- 
      NBlist[[s]]$perf %>% 
      mutate(method = "Naive Bayes")
    perf_CART <- 
      CARTlist[[s]]$perf %>% 
      mutate(method = "CART")
    
    # Get BART performance on test data ----
    temp <- tibble(
      p_BART = NBlist[[s]]$dat_test$p_BART,
      y_BART = factor(ifelse(p_BART > 0.5, 1, 0),levels=c("1", "0")),
      y = NBlist[[s]]$dat_test$y
    )
    spec <- yardstick::spec
    sens <- yardstick::sens
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy)
    perf_BART <- 
      mlist(temp, truth=y, estimate=y_BART) %>% 
      mutate(
        method = "BART",
        surgery = s
      )
    
    pltdat <- bind_rows(perf_BART, perf_NB, perf_CART) %>% 
      select(-.estimator) %>% 
      filter(.metric %in% c("sens", "spec", "bal_accuracy")) %>% 
      mutate(
        .metric = recode(.metric, 
                         "bal_accuracy" = "BalAcc",
                         "sens" = "Sens",
                         "spec" = "Spec")
      )
    
    # Make the plot ----
    perfplot <- 
      ggplot(pltdat, aes(x=.metric, y=.estimate, fill=method)) +
      geom_col(lwd=.05, color="grey5", position = position_dodge(width=.6), width=.6) +
      labs(
        title = glue("{vlabs[s]}"),
        x = "",
        y = ""
      ) +
      coord_cartesian(ylim = c(0, 1.0)) +
      scale_fill_discrete_diverging(l2=65) +
      theme_minimal() +
      theme(
        plot.title.position = "plot",
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "lines"),
        legend.spacing.y = unit(0.1, "cm"),
        axis.text.x = element_text(size = rel(.5)),
        axis.text.y = element_text(size = rel(.5), face = "bold"),
        title = element_text(size = rel(.5), face = "bold"),
        plot.margin = margin(5, 10, 0, 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey85", linewidth=.1)
      )
    
    return(list(plt = perfplot, perf = pltdat))
  }

# ---- Function to compare BART to Naive Bayes ----
perf_compare_NB_BART <- 
  function(mod_list, s){
    # Get NB and BART predictions -----
    temp <- 
      mod_list[[s]]$dat_test %>% 
      mutate(
        y_BART = factor(ifelse(p_BART > 0.5, 1, 0),levels=c("1", "0")),
        y_Naive = factor(ifelse(p_Naive > 0.5, 1, 0),levels=c("1", "0")),
      ) 
    
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy)
    perf_BART <- mlist(temp, truth=y, estimate=y_BART) %>% mutate(method = "BART")
    perf_Naive <- mlist(temp, truth=y, estimate=y_Naive) %>% mutate(method = "Naive Bayes")
    
    # Combine performance ----
    perf_both <- 
      bind_rows(perf_BART, perf_Naive) %>% 
      filter(.metric %in% c("sens", "spec", "bal_accuracy")) %>% 
      select(-.estimator) %>% 
      mutate(prev = sum(mod_list[[s]]$dat_test$y==1)/nrow(mod_list[[s]]$dat_test)) 
    
    perfplot <- 
      ggplot(perf_both, aes(x=.metric, y=.estimate, fill=method)) +
      geom_col(position = position_dodge(width=.6), width=.6) +
      labs(
        title = glue("{vlabs[s]}"),
        x = "",
        y = ""
      ) +
      coord_cartesian(ylim = c(0.5, 1.0)) +
      theme_minimal() +
      theme(
        plot.title.position = "plot",
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "lines"),
        legend.spacing.y = unit(0.1, "cm"),
        axis.text.x = element_text(size = rel(.5)),
        axis.text.y = element_text(size = rel(.5), face = "bold"),
        title = element_text(size = rel(.5), face = "bold"),
        plot.margin = margin(5, 10, 0, 10),
      )
    
    return(list(plt = perfplot, perf = perf_both))
  }

# ---- Format patient data for Naive Bayes ----
get_patient_data_NB <- 
  function(xpt, s){
    vlist <- get_prop_model_vars(s)
    
    # Get patient data -----
    obs <- 
      ptdat$xpt %>% 
      select(all_of(vlist)) %>%
      mutate(
        across(
          where(is.numeric),
          ~ as.double(.)
        )
      ) %>%
      mutate(
        across(
          where(~ !is.numeric(.) && any(is.na(.))),
          as.numeric
        )
      ) %>% 
      mutate(
        across(
          where(is.factor),
          ~ factor(., levels = levels(dat[[cur_column()]]))
        )
      )
  }

pred_pt_surg <- 
  function(mod_list, s, xpt){
    obs <- get_patient_data_NB(xpt, s)
    mod <- mod_list[[s]]$mod
    ypt_prob <- predict(mod, newdata = obs, type = "prob")[, 1]
    return(ypt_prob)
  } 

# ---- Function to get log-odds ratio contribution scores for Naive Bayes ----
get_contribution_logodds <- 
  function(obss, mod, df_tr, df_te, vlist, s, class_test) {
    surg <- glue("interval_{s}")
    feature_contributions <- numeric(ncol(obss))
    names(feature_contributions) <- names(obss)
    
    for (feature in names(obss)) {
      feature_data <- mod$tables[[feature]]
      obs_value <- obss[[feature]]
      
      # Feature is numeric -----
      if (is.numeric(obs_value)) {
        if (inherits(feature_data$`1`, "density")) {
          # Evaluate the kernel density at the observed value
          if(class_test == 1) {
            feature_data <- feature_data$`1`
          } else {
            feature_data <- feature_data$`0`
          }
          iclose <- which.min(abs(feature_data$x - obs_value))
          
          # If the observed value  outside range (e.g., NA), set likelihood to NA
          if(length(iclose) > 0) {
            likelihood <- feature_data$y[iclose]
          } else {
            likelihood <- NA
          }
          
          # Handle cases where the density might be effectively zero -----
          if (likelihood > 0 & !is.na(likelihood)) {
            feature_contributions[feature] <- log(likelihood)
          } else {
            feature_contributions[feature] <- -Inf
          }
        }
        
        # Feature is categorical -----
      } else if (is.factor(obs_value)) { 
        # Categorical feature (remains the same logic)
        if (as.character(obs_value) %in% rownames(feature_data)) {
          prob <- feature_data[as.character(obs_value), as.character(class_test)]
          if (!is.na(prob) && prob > 0) {
            feature_contributions[feature] <- log(prob)
          } else {
            feature_contributions[feature] <- -Inf
          }
        } else {
          feature_contributions[feature] <- -Inf # Unseen level
        }
      }
    }
    
    # ---- Build contribution data ----
    contribution_df <- 
      tibble(
        feature = vlabs[names(feature_contributions)],
        log_likelihood = feature_contributions
      ) %>% 
      mutate(
        feature = fct_inorder(feature),
        class_test = fct(as.character(class_test), levels = levels(df_te$y)),
      )
    
    return(contribution_df)
  }

# ---- Get log-odds contribution from Naive Bayes ----
get_pdiff <- 
  function(mod_list, df_list, side, s){
    mod <- mod_list[[s]]$mod
    
    df_tr <- mod_list[[s]]$dat_train
    df_te <- mod_list[[s]]$dat_test
    
    obs <- get_patient_data_NB(xpt, s)
    sideN <- ifelse(side=="Left", 1, 2)
    
    p0 <- get_contribution_logodds(obs[sideN,], mod, df_tr, df_te, vlist, s, 0)
    p1 <- get_contribution_logodds(obs[sideN,], mod, df_tr, df_te, vlist, s, 1)
    
    pdiff <- tibble(
      feature = p0$feature,
      log_likelihood = p1$log_likelihood - p0$log_likelihood,
      side = side
    )
    
    return(pdiff)
  }

# ---- Make log-odds feature contribution plot for Naive Bayes ----
contribution_plot_logodds <- 
  function(mod_list, df_list, side, s, ypt_prob){
    pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
    
    # Propensities for header ----
    probL <- glue("{as.character(round(100 * ypt_prob[[s]][1]))}%")
    probR <- glue("{as.character(round(100 * ypt_prob[[s]][2]))}%")
    probside <- ifelse(side == "Left", probL, probR)
    
    # Log-likelihood difference between classes ----
    diffside <- 
      get_pdiff(mod_list, df_list, side, s) %>% 
      mutate(side = glue("{side} ({probside})")) %>% 
      arrange(log_likelihood) %>% 
      mutate(feature = fct_inorder(str_wrap(feature, 25)))
    
    ggplot(diffside, aes(y=feature, x=log_likelihood, fill=log_likelihood)) +
      geom_col() +
      geom_vline(xintercept = 0, color = "grey10", lwd=.8) +
      facet_wrap(~side) +
      labs(x = "Contraindication                  Indication", 
           y = "") +
      scale_x_continuous(
        breaks = seq(-3, 3),
        labels = c("Strong", "Medium", "Weak", "None", "Weak", "Medium", "Strong"),
      ) + 
      coord_cartesian(xlim = c(-3,3)) +
      scale_fill_continuous_diverging(
        palette = "Purple-Green",
        limits = c(-log(10), log(10)),
        labels = c("Strong", "Medium", "Weak", "None", "Weak", "Medium", "Strong"),
        breaks = c(-log(10), -log(5), -log(2), 0, log(2), log(5), log(10)),
        oob = scales::squish
      ) +
      theme_minimal() +
      theme(
        plot.title.position = "plot",
        legend.position = "none",
        axis.text.y = element_text(size = rel(.4)),
        axis.text.x = element_text(size = rel(.4), face = "bold"),
        strip.text = element_text(size = rel(.4), face = "bold"),
        plot.title = element_text(size = rel(.3)),
        axis.title = element_text(size = rel(.4)),
        plot.margin = margin(5, 10, 5, 10),
      )
  }

# ---- Function to generate Shapley table for Naive Bayes ----
prop_shap_table_NB <- 
  function(xpt, s, mod_list){
    smart_round <- function(x, small_digits = 2) {
      case_when(
        abs(x) < 0.001 ~ round(x, 3),
        abs(x) < 0.01 ~ round(x, 2),
        abs(x) < 0.1 ~ round(x, 1),
        is.na(x) ~ NA_real_,
        TRUE ~ round(x, 0)
      )
    }
    
    # Get patient data ----
    obs <- get_patient_data_NB(xpt, s)
    xL <- obs[1,] %>% mutate(across(where(is.numeric), ~ smart_round(.)))
    xR <- obs[2,] %>% mutate(across(where(is.numeric), ~ smart_round(.)))
    
    # Get model and data ----
    mod <- mod_list[[s]]$mod
    df <- mod_list[[s]]$dat_train %>% select(names(mod$data$x))
    
    # Compute Shapley Values ----
    shapmod <- Predictor$new(mod, data = df)
    set.seed(42)
    shapL <- Shapley$new(shapmod, x.interest = xL, sample.size = 100)
    shapR <- Shapley$new(shapmod, x.interest = xR, sample.size = 100)
    shapL$results <- shapL$results %>% filter(class == 1)
    shapR$results <- shapR$results %>% filter(class == 1)
    
    # Make Labels for shapley table ----
    makelab <- function(shapres, xside){
      for (f in shapres$results$feature) {
        val <- xside[[f]]
        gaitvbl <- str_detect(f, "^ic|^fo|^ofo|^ofc|^mean|^min|^max|^t_|^mids|^rom")
        lenvbl <- str_detect(f, "len$")
        velvbl <- str_detect(f, "vel$")
        if(is.character(val)) {
          val <- val
        } else {
          val <- case_when(
            gaitvbl & !(lenvbl | velvbl) ~ as.character(val),
            lenvbl ~ as.character(val),
            velvbl ~ sprintf("%.1e", val),
            TRUE ~ as.character(val)
          )
        }
        
        # Add label - first check for emoji version, then plain text, then blank
        lab <- varlabs$Labelx[varlabs$Variable == f]
        lab <- ifelse(is.na(lab), varlabs$Label[varlabs$Variable == f], lab)
        lab <- ifelse(is.na(lab), f, lab)
        lab <- str_replace_all(lab, "_", " ")
        labx <- str_wrap(glue("{lab} = {val}"), 35)
        
        shapres$results$label[shapres$results$feature == f] <- labx
        shapres$results$lab[shapres$results$feature == f] <- lab
        shapres$results$value[shapres$results$feature == f] <- as.character(val)
      }   
      return(shapres)
    }
    
    shapL <- makelab(shapres=shapL, xside=xL)
    shapR <- makelab(shapR, xR)
    
    # Arrange data ----
    arrangeshap <- function(shapres) {
      temp <-
        shapres$results %>%
        arrange(desc(phi)) %>%
        mutate(
          feature.value = str_replace(feature.value, "=", " = "),
          feature.value = str_replace_all(feature.value, "_", " ")
        ) %>%
        mutate(feature.value = fct_rev(fct_inorder(feature.value))) %>%
        mutate(label = fct_rev(fct_inorder(label)))
    }
    shapL <- arrangeshap(shapL)
    shapR <- arrangeshap(shapR)
    
    # Make Shapley value tables ----
    tblL <- 
      shapL %>% 
      select(phi, lab, value) %>% 
      mutate(SIDE = "L") %>% 
      arrange(desc(phi)) %>% 
      rownames_to_column(var="imp") %>% 
      mutate(imp = as.numeric(imp)) %>% 
      relocate(phi) %>% 
      mutate(phi = round(phi,2)) %>% 
      rename(phix = phi)
    
    tblR <- 
      shapR %>% 
      select(phi, lab, value) %>% 
      mutate(SIDE = "R") %>% 
      arrange(desc(phi)) %>% 
      rownames_to_column(var="imp") %>% 
      mutate(imp = as.numeric(imp)) %>% 
      relocate(phi) %>% 
      mutate(phi = round(phi,2)) %>% 
      rename(phix = phi)
    
    tblshap <- bind_rows(tblL, tblR) %>% mutate(Surgery = vlabs[s])
    
    return(tblshap)
  } 

# ---- Function to generate Shapley table for CART ----
prop_shap_table_CART <- 
  function(xpt, s, mod_list){
    smart_round <- function(x, small_digits = 2) {
      case_when(
        abs(x) < 0.001 ~ round(x, 3),
        abs(x) < 0.01 ~ round(x, 2),
        abs(x) < 0.1 ~ round(x, 1),
        is.na(x) ~ NA_real_,
        TRUE ~ round(x, 0)
      )
    }
    
    # Get patient data ----
    obs <- get_patient_data_NB(xpt, s)
    xL <- obs[1,] %>% mutate(across(where(is.numeric), ~ smart_round(.)))
    xR <- obs[2,] %>% mutate(across(where(is.numeric), ~ smart_round(.)))
    
    # Get model and data ----
    mod <- mod_list[[s]]$mod
    df <- mod_list[[s]]$dat_train %>% select(all_of(names(obs)))
    
    # Compute Shapley Values ----
    shapmod <- Predictor$new(mod, data = df)
    set.seed(42)
    shapL <- Shapley$new(shapmod, x.interest = xL, sample.size = 100)
    shapR <- Shapley$new(shapmod, x.interest = xR, sample.size = 100)
    shapL$results <- shapL$results %>% filter(class == 1)
    shapR$results <- shapR$results %>% filter(class == 1)
    
    # Make Labels for shapley table ----
    makelab <- function(shapres, xside){
      for (f in shapres$results$feature) {
        val <- xside[[f]]
        gaitvbl <- str_detect(f, "^ic|^fo|^ofo|^ofc|^mean|^min|^max|^t_|^mids|^rom")
        lenvbl <- str_detect(f, "len$")
        velvbl <- str_detect(f, "vel$")
        if(is.character(val)) {
          val <- val
        } else {
          val <- case_when(
            gaitvbl & !(lenvbl | velvbl) ~ as.character(val),
            lenvbl ~ as.character(val),
            velvbl ~ sprintf("%.1e", val),
            TRUE ~ as.character(val)
          )
        }
        
        # Add label - first check for emoji version, then plain text, then blank
        lab <- varlabs$Labelx[varlabs$Variable == f]
        lab <- ifelse(is.na(lab), varlabs$Label[varlabs$Variable == f], lab)
        lab <- ifelse(is.na(lab), f, lab)
        lab <- str_replace_all(lab, "_", " ")
        labx <- str_wrap(glue("{lab} = {val}"), 35)
        
        shapres$results$label[shapres$results$feature == f] <- labx
        shapres$results$lab[shapres$results$feature == f] <- lab
        shapres$results$value[shapres$results$feature == f] <- as.character(val)
      }   
      return(shapres)
    }
    
    shapL <- makelab(shapres=shapL, xside=xL)
    shapR <- makelab(shapR, xR)
    
    # Arrange data ----
    arrangeshap <- function(shapres) {
      temp <-
        shapres$results %>%
        arrange(desc(phi)) %>%
        mutate(
          feature.value = str_replace(feature.value, "=", " = "),
          feature.value = str_replace_all(feature.value, "_", " ")
        ) %>%
        mutate(feature.value = fct_rev(fct_inorder(feature.value))) %>%
        mutate(label = fct_rev(fct_inorder(label)))
    }
    shapL <- arrangeshap(shapL)
    shapR <- arrangeshap(shapR)
    
    # Make Shapley value tables ----
    tblL <- 
      shapL %>% 
      select(phi, lab, value) %>% 
      mutate(SIDE = "L") %>% 
      arrange(desc(phi)) %>% 
      rownames_to_column(var="imp") %>% 
      mutate(imp = as.numeric(imp)) %>% 
      relocate(phi) %>% 
      mutate(phi = round(phi,2)) %>% 
      rename(phix = phi)
    
    tblR <- 
      shapR %>% 
      select(phi, lab, value) %>% 
      mutate(SIDE = "R") %>% 
      arrange(desc(phi)) %>% 
      rownames_to_column(var="imp") %>% 
      mutate(imp = as.numeric(imp)) %>% 
      relocate(phi) %>% 
      mutate(phi = round(phi,2)) %>% 
      rename(phix = phi)
    
    tblshap <- bind_rows(tblL, tblR) %>% mutate(Surgery = vlabs[s])
    
    return(tblshap)
  } 

# ---- Format Shapley values for Naive Bayes ----
fmt_shapLR_NB <- 
  function(tall, ptprops, s){
    tit <- str_replace_all(vlabs[s], "_", " ")
    t <- tall[[s]]
    
    p <- 
      ptprops %>% 
      filter(Surgery == str_replace_all(vlabs[s], "_", " ")) %>%
      mutate(SIDE = ifelse(SIDE == "Left", "L", "R"))
    
    pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
    
    pfill <- function(x){
      y <- case_when(
        x < 20 ~ pal[1],
        x < 40 ~ pal[2],
        x < 60 ~ pal[3],
        x < 80 ~ pal[4],
        TRUE ~ pal[5]
      )
    }
    pcolor <- function(x){
      y <- case_when(
        x < 20 ~ "white",
        x < 40 ~ "grey20",
        x < 60 ~ "grey20",
        x < 80 ~ "grey20",
        TRUE ~ "white"
      )   
    }
    pfillL <- pfill(p$q50[p$SIDE == "L"])
    pfillR <- pfill(p$q50[p$SIDE == "R"])
    pcolorL <- pcolor(p$q50[p$SIDE == "L"])
    pcolorR <- pcolor(p$q50[p$SIDE == "R"])
    
    # Source note text ----
    source_note_html <- htmltools::HTML("<span style='font-style: italic;'>Indications: <span style='color: #005600; font-weight: bold'>strong</span>, <span style='color: #ABDFAC; font-weight: bold'>weak</span>. Counterindications <span style='color: #841859; font-weight: bold'>strong</span>, <span style='color: #FFBFDE; font-weight: bold'>weak</span></span>")
    
    tbl <- 
      t %>%  
      mutate(
        phix = cut(phix, c(-1, -.2, -.05, -.01, .01, .05, .2, 1), 
                   labels = c(1, 2, 3, 4, 5, 6, 7))
      ) %>% 
      pivot_wider(
        names_from = SIDE, 
        values_from = c(phix, imp, lab, value), 
        values_fn = list
      ) %>%  
      mutate(blank = " ") %>% 
      unnest(cols = everything()) %>% 
      select(-Surgery, -imp_L, -imp_R) %>% 
      relocate(phix_L, lab_L, value_L, blank, phix_R, lab_R, value_R)
    
    tLR <- 
      tbl %>% 
      gt() %>% 
      tab_header(title = tit) %>%  
      opt_align_table_header(align = "left") %>% 
      cols_hide(starts_with("phi")) %>% 
      cols_label(
        lab_L = "Characteristic",
        lab_R = "Characteristic",
        value_L = "Value",
        value_R = "Value",
      ) %>% 
      cols_width(
        starts_with("value") ~ px(145),
        starts_with("lab") ~ px(230),
        "blank" ~ px(20)
      ) %>% 
      tab_style(
        style = cell_text(color = "white"),
        locations = cells_column_labels(columns = "blank")
      ) |> 
      tab_spanner(
        label =  glue("Left Propensity = {p$q50[1]}% ({p$q5[1]}%, {p$q95[1]}%)"),
        columns = c(lab_L, value_L), 
        id = "L" 
      ) %>%  
      tab_spanner(
        label = glue("Right Propensity = {p$q50[2]}% ({p$q5[2]}%, {p$q95[2]}%)"),
        columns = c(lab_R, value_R),
        id = "R"
      ) %>% 
      tab_style(
        style = list(cell_fill(color = pfillL), cell_text(color = pcolorL)),
        locations = cells_column_spanners(spanners = "L")
      ) %>% 
      tab_style( 
        style = list(cell_fill(color = pfillR), cell_text(color = pcolorR)),
        locations = cells_column_spanners(spanners = "R")
      ) %>%  
      tab_style(
        style = cell_text(weight = "bold"),
        locations = list(
          cells_column_labels(),
          cells_column_spanners(),
          cells_title(groups = c("title"))
        )
      ) %>% 
      tab_style(
        style = cell_text(style = "italic"),
        locations = list(
          cells_column_spanners()
        )
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = list(
          cells_body(columns = starts_with("val")),
          cells_column_labels(starts_with("val"))
        )
      ) %>% 
      data_color(
        columns = phix_L,
        method = "factor",
        target_columns = c(lab_L, value_L),
        palette = diverging_hcl(n = 7, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8),
        domain = c(1, 2, 3, 4, 5, 6, 7),
        autocolor_text = T
      ) %>% 
      data_color(
        columns = phix_R,
        method = "factor",
        target_columns = c(lab_R, value_R),
        palette = diverging_hcl(n = 7, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8),
        domain = c(1, 2, 3, 4, 5, 6, 7),
        autocolor_text = T  
      )
  }

# ---- Get threshold for outcome variables with unknown direction ----
get_outcome_vunk <- 
  function(df, vv){
    
    temp <- get_outcome_thresh(vv, df[1,])[1]
    
    # Guess signs for BIMAL ----
    if(vv == "BIMAL"){
      sgn <- case_when(
        df$BIMAL > 30 ~ -1,
        df$BIMAL < 0 ~ 1,
        TRUE ~ 1
      )
    }
    
    # Guess signs for ANTEVERSION ----
    if(vv == "ANTEVERSION"){
      sgn <- case_when(
        df$ANTEVERSION < 10 ~ 1,
        TRUE ~ -1
      )
    }
    
    # Guess signs for Kne_Ang_Trn ----
    if(vv == "meansta_Kne_Ang_Trn"){
      sgn <- case_when(
        df$meansta_Kne_Ang_Trn > 8 ~ -1,
        df$meansta_Kne_Ang_Trn < -27 ~ 1,
        TRUE ~ 1
      )
    }
    
    # Guess signs for Hip_Ang_Trn ----
    if(vv == "meansta_Hip_Ang_Trn"){
      sgn <- case_when(
        df$meansta_Hip_Ang_Trn < -13 ~ 1,
        TRUE ~ -1
      )
    }
    
    # Guess signs for Kne_Ang_Trn ----
    if(vv == "meansta_Foo_Ang_Trn"){
      sgn <- case_when(
        df$meansta_Foo_Ang_Trn > 11 ~ -1,
        df$meansta_Foo_Ang_Trn < -31 ~ 1,
        TRUE ~ -1
      )
    }
    
    thresh <- temp * sgn
  }

# ---- Build categorical outcome models with Naive Bayes ----
model_outcome_NB <- 
  function(df, outbart, v){
    # Outcome variables with unknown direction -----
    vunk <- c("ANTEVERSION", "BIMAL", "meansta_Hip_Ang_Trn",
              "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
    
    # Get predictors ----
    vlist <- names(outbart[[v]]$mod$X)
    
    # Get thresholds ----
    if(!(v %in% vunk)){
      thresh <- get_outcome_thresh(v, df[1,])[1] * get_outcome_thresh(v, df[1,])[2]
    } else{
      thresh <-  get_outcome_vunk(df, v)
    }
    
    # Get outcome ----
    y <- df[[glue("{v}Post")]] - df[[v]]
    y <- case_when(
      thresh > 0 & y < thresh ~ "Poor",
      thresh > 0 & y >= thresh ~ "Good",
      thresh < 0 & y < thresh ~ "Good",
      thresh < 0 & y >= thresh ~ "Poor"
    )
    df$y <- factor(y, levels = c("Poor", "Good"))
    
    # Suppress warnings ----
    options(warn = -1)
    
    # Get data ----
    dat_train <- suppressMessages(inner_join(df, outbart[[v]]$mod$X))
    dat_test <- setdiff(df, dat_train)
    dat_train <- dat_train %>% drop_na(y, all_of(v))
    dat_test <- dat_test %>% drop_na(y, all_of(v))
    
    # Build the Naive Bayes model ----
    mod <- 
      naive_bayes(
        y ~ .,
        data = dat_train %>% select(all_of(vlist), y),
        laplace = 0.5
      )
    
    # Evaluate the model ----
    y_pred <- predict(mod, newdata = dat_test)
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,2]
    pred <- tibble(
      y_meas = dat_test$y,
      y_pred = y_pred,
      good_prob = y_prob
    )
    
    # Percent of "Good" outcome ----
    pct_good <- round(sum(dat_test$y == "Good") / nrow(dat_test), 2)
    
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=y_meas, estimate=y_pred) %>% select(-.estimator)
    perf$var <- v
    perf$model <- "Naive Bayes"
    perf$pct_good <- pct_good
    perf$thresh <- unique(abs(thresh))
    perf <- perf %>% mutate(.estimate = round(.estimate, 2))
    
    # Reinstate warnings ----
    options(warn = 0)
    
    return(list(mod = mod, perf = perf, pred = pred, df = dat_test))
  }

# ---- Build categorical outcome models with CART ----
model_outcome_CART <- 
  function(df, outbart, v){
    # Outcome variables with unknown direction -----
    vunk <- c("ANTEVERSION", "BIMAL", "meansta_Hip_Ang_Trn",
              "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
    
    # Get predictors ----
    vlist <- names(outbart[[v]]$mod$X)
    
    # Get thresholds ----
    if(!(v %in% vunk)){
      thresh <- get_outcome_thresh(v, df[1,])[1] * get_outcome_thresh(v, df[1,])[2]
    } else{
      thresh <-  get_outcome_vunk(df, v)
    }
    
    # Get outcome ----
    y <- df[[glue("{v}Post")]] - df[[v]]
    y <- case_when(
      thresh > 0 & y < thresh ~ "Poor",
      thresh > 0 & y >= thresh ~ "Good",
      thresh < 0 & y < thresh ~ "Good",
      thresh < 0 & y >= thresh ~ "Poor"
    )
    df$y <- factor(y, levels = c("Poor", "Good"))
    
    # Suppress warnings ----
    options(warn = -1)
    
    # Get data -----
    dat_train <- suppressMessages(inner_join(df, outbart[[v]]$mod$X))
    dat_test <- setdiff(df, dat_train)
    vlist <- names(outbart[[v]]$mod$X)
    nsamp <- nrow(dat_train)
    
    # Define classifier and workflow -----
    tree_spec <- 
      decision_tree(
        cost_complexity = tune(),  # `cp` in rpart
        tree_depth = tune(),       # `maxdepth` in rpart
        min_n = tune()             # `minsplit` in rpart
      ) %>%
      set_engine("rpart") %>%
      set_mode("classification")
    
    # Get data for cross-validation folds -----
    Ns <- sum(dat_train$y=="Good")
    dat_bal <- slice_sample(dat_train, n = Ns, by = y)
    
    dat_cv <-
      dat_train %>%
      select(all_of(vlist), y)
    cv_folds <- vfold_cv(dat_cv, v = 10) 
    
    # Build workflow -----
    tree_wf <- 
      workflow() %>%
      add_model(tree_spec) %>%
      add_formula(y ~ .)
    
    # Grid Search: Random search is fast and works with flat cost -----
    set.seed(42)
    tree_grid <- 
      grid_random(
        cost_complexity(range = c(-4, 0)),
        tree_depth(range = c(2, 6)),
        min_n(range = c(round(nsamp/20), round(nsamp/10))),
        size = 50
      )
    
    tree_results <- 
      tune_grid(
        tree_wf,
        resamples = cv_folds,
        grid = tree_grid,
        metrics = metric_set(bal_accuracy, sens, spec, npv, ppv, f_meas)
      )
    
    # Choose optimal parameters -----
    mopt <- "bal_accuracy"
    best_params <- select_best(tree_results, metric = mopt)
    final_tree <- finalize_model(tree_spec, best_params)
    final_wf <- 
      workflow() %>%
      add_model(final_tree) %>%
      add_formula(y ~ .)
    
    # Train final model ----
    final_fit <- fit(final_wf, data = dat_cv)
    mod <- final_fit %>% extract_fit_engine()
    
    # Plot final model -----
    rpart.plot(mod, type = 5, varlen = 14, faclen=9, cex = .6, roundint = F)
    
    # Evaluate the model ----
    y_meas  <-  dat_test$y
    y_pred <- predict(mod, newdata = dat_test, type = "class")
    y_prob <- predict(mod, newdata = dat_test, type = "prob")[,1]
    dat_test$p_CART <- y_prob
    pred <- tibble(truth=y_meas, pred_class=y_pred, class1_prob=y_prob)
    
    # Percent of "Good" outcome ----
    pct_good <- round(sum(dat_test$y == "Good") / nrow(dat_test), 2)
    
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=truth, estimate=pred_class) %>% select(-.estimator)
    perf$var <- v
    perf$model <- "CART"
    perf$pct_good <- pct_good
    perf$thresh <- unique(abs(thresh))
    perf <- perf %>% mutate(.estimate = round(.estimate, 2))
    
    # Reinstate warnings ----
    options(warn = 0)
    
    return(list(mod = mod, perf = perf, pred = pred, df = dat_test))
  }

# ---- Build categorical outcome models from BART models ----
model_outcome_BART <- 
  function(outbart, df, v){
    # Suppress warnings -----
    options(warn = -1)
    
    # Remove observations with missing y, yPost ----
    df <- df %>% drop_na(all_of(v), all_of(glue("{v}Post")))
    
    # Outcome variables with unknown direction ----
    vunk <- c("ANTEVERSION", "BIMAL", "meansta_Hip_Ang_Trn",
              "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
    # Get predictors ----
    vlist <- names(outbart[[v]]$mod$X)
    
    # Get thresholds ----
    if(!(v %in% vunk)){
      thresh <- get_outcome_thresh(v, df[1,])[1] * get_outcome_thresh(v, df[1,])[2]
      thresh <- rep(thresh, nrow(df))
    } else{
      thresh <-  get_outcome_vunk(df, v)
    }
    
    # Get outcome ----
    diff <- df[[glue("{v}Post")]] - df[[v]]
    
    # ---- Function to comptue outcome based on y and thresh ----
    yfun <- function(diff, thresh){
      thresh.gt.zero <- (thresh > 0)
      diff.gt.thresh <- (diff > thresh)
      good1 <- thresh.gt.zero & diff.gt.thresh
      poor1 <- thresh.gt.zero & !diff.gt.thresh
      good2 <- !thresh.gt.zero & !diff.gt.thresh
      poor2 <- !thresh.gt.zero & diff.gt.thresh
      res <- ifelse(poor1 | poor2, "Poor", "Good")
      return(res)
    }
    
    df$y <- factor(yfun(diff, thresh), levels = c("Poor", "Good"))
    
    # Choose model ----
    mod <- outbart[[v]]$mod
    temp <- outbart[[v]]$testperf
    X <- mod$X
    vv <- names(X)
    
    # Get data ----
    dat_train <- suppressMessages(inner_join(df, X))
    dat_test <- setdiff(df, dat_train) %>% drop_na(all_of(v))
    thresh_test <- thresh[dat_test$rowid]
    
    # Get outcome ----
    temp <- bart_machine_get_posterior(mod, dat_test %>% select(all_of(vv)))
    out_pred <- temp$y_hat
    out_meas <- dat_test[[glue("{v}Post")]] - dat_test[[v]]
    
    # Get outcome > thresh probability ----
    post <- temp$y_hat_posterior_samples
    
    # Get probability of good outcome based on threshold sign ----
    postx <- post * sign(thresh_test)
    y_prob <- rowSums(postx > abs(thresh_test)) / nrow(postx)
    
    pred <- tibble(
      y_meas = dat_test$y,
      # TODO: need to fix this vvvv
      y_pred = factor(y_prob >= .5, levels = c("FALSE", "TRUE"), labels=c("Poor","Good")),
      good_prob = y_prob
    )
    
    # Percent of "Good" outcome ----
    pct_good <- round(table(pred$y_meas)["Good"]/ nrow(pred), 2)
    
    # Compute sens, specy, ppv, npv, accuracy using yardstick metrics ----
    spec <- yardstick::spec
    mlist <- metric_set(sens, spec, ppv, npv, bal_accuracy, f_meas)
    perf <- mlist(pred, truth=y_meas, estimate=y_pred)
    perf$var <- v
    perf$model <- "BART"
    perf$pct_good <- pct_good
    perf$thresh <- unique(abs(thresh))
    perf <- perf %>% mutate(.estimate = round(.estimate, 2))
    
    # Reinstate warnings ----
    options(warn = 0)
    
    return(list(mod = mod, perf = perf, pred = pred))
  }

# ---- Compare outcome probab distributions between Naive Bayes and Bart ----
out_prob_plot <- 
  function(out_list_BART, out_list_Naive, vtest){
    
    pb <- out_list_BART[[vtest]]$pred$good_prob
    yb <- out_list_BART[[vtest]]$pred$y_meas
    mb <- "BART"
    pn <- out_list_Naive[[vtest]]$pred$good_prob
    yn <- out_list_Naive[[vtest]]$pred$y_meas
    mn <- "Naive Bayes"
    
    tb <- tibble(p=pb, y=yb, m=mb) %>% arrange(p) %>% mutate(id = row_number())
    tn <- tibble(p=pn, y=yn, m=mn) %>% arrange(p) %>% mutate(id = row_number())
    pdat <- bind_rows(tb, tn)
    
    ggplot(pdat %>% drop_na(), aes(x=id, y=p, color=y)) + 
      geom_jitter(shape=1,height=.05, size=.7) + 
      geom_hline(yintercept = .5) +
      facet_wrap(~m) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title=vlabs[vtest], x="", y="Probability of Good Outcome") +
      scale_x_discrete(labels = NULL) +
      scale_color_discrete_diverging(palette = "Purple-Green", l1 = 45, c1 = 60,
                                     guide = guide_legend(title = "Outcome")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(.5), face = "bold"),
        axis.title = element_text(size = rel(.5), face = "bold"),
        axis.text = element_text(size = rel(.5)),
        strip.text = element_text(size = rel(.5), face = "italic"),
      )
  }

# ---- Get outcome thresholds for patient ----
get_thresh_pt <- 
  function(voutlist, xx){
    threshlist <- voutlist %>% map(get_outcome_thresh, xx)
    names(threshlist) <- voutlist
    threshlist <- 
      threshlist %>% 
      imap_dfr(~ tibble(name = .y, value1 = .x[1], value2 = .x[2])) %>% 
      rename(val = value1, sign = value2) %>% 
      mutate(thresh = ifelse(sign == 0, val, sign*val)) %>% 
      select(-val, -sign)
    threshL <- setNames(threshlist$thresh, threshlist$name)
  }

# ---- BART: Predict outcomes for a given surgery s ----
pred_pt_outcome_BART <- function(xx=xpt, s, out_list_BART){
  # ---- Build control x ----
  x_control <- 
    xx %>% 
    mutate(
      across(
        starts_with("interval_"),
        ~ factor("0", levels = c("1", "0"))
      ),
      status = "Control",
    )
  
  # ---- Build treated x ----
  x_treated <- 
    x_control %>% 
    mutate(
      !!sym(glue("interval_{s}")) := factor("1", levels = c("1", "0")),
      status = "Treated"
    )
  x_sim <- bind_rows(x_treated, x_control)
  
  # ---- Get outcome variables ----
  voutlist <- get_outcome_vars(s)
  
  # ---- Get Treated and Control outcomes for one variable ----
  get_outcome_v1 <- function(out_list_BART, v, x_sim){
    # Choose model ----
    mod <- out_list_BART[[v]]$mod
    X <- mod$X
    
    # Get outcome ----
    temp <- bart_machine_get_posterior(mod, x_sim %>% select(all_of(names(X))))
    out_pred <- temp$y_hat
    post <- temp$y_hat_posterior_samples
    
    # Get threshold for v ----
    threshL <- get_outcome_thresh(v, x_sim[1,])
    threshL <- threshL[1]*threshL[2]
    threshR <- get_outcome_thresh(v, x_sim[2,])
    threshR <- threshR[1]*threshR[2]
    thresh <- c(threshL, threshR, threshL, threshR)
    
    # Assign probability -----
    y_prob <- case_when(
      thresh > 0 ~ rowSums(post > thresh) / ncol(post),
      thresh < 0 ~ rowSums(post < thresh) / ncol(post)
    )
    res <- tibble(
      surgery = vlabs[s],
      var = vlabs[v],
      thresh = thresh,
      status = x_sim$status,
      side = rep(c("Left", "Right"), 2),
      y_value = out_pred,
      good_prob = round(100 * y_prob),
      method = "BART"
    )
  }
  
  # Get outcome for all variables ----
  res <- voutlist %>% 
    map(get_outcome_v1, out_list_BART=out_list_BART, x_sim=x_sim) %>% 
    bind_rows() 
  return(res)
}

# ---- CART: Predict outcomes for a given surgery s ----
pred_pt_outcome_CART <- function(xx=xpt, s, out_list_CART){
  # ---- Build control x ----
  x_control <- 
    xx %>% 
    mutate(
      across(
        starts_with("interval_"),
        ~ factor("0", levels = c("1", "0"))
      ),
      status = "Control",
    )
  
  # ---- Build treated x ----
  x_treated <- 
    x_control %>% 
    mutate(
      !!sym(glue("interval_{s}")) := factor("1", levels = c("1", "0")),
      status = "Treated"
    )
  x_sim <- bind_rows(x_treated, x_control)
  
  # ---- Get outcome variables ----
  voutlist <- get_outcome_vars(s)
  
  # ---- Get Treated and Control outcomes for one variable ----
  get_outcome_v1 <- function(out_list_CART, v, x_sim){
    # Choose model ----
    mod <- out_list_CART[[v]]$mod
    
    # Get outcome ----
    y_prob <- predict(mod, x_sim[attr(mod$terms, "term.labels")], type = "prob")[,2]
    y_pred <- factor(y_prob >= .5, levels = c("FALSE", "TRUE"), labels=c("Poor","Good"))
    
    # Get threshold for v ----
    threshL <- get_outcome_thresh(v, x_sim[1,])
    threshL <- threshL[1]*threshL[2]
    threshR <- get_outcome_thresh(v, x_sim[2,])
    threshR <- threshR[1]*threshR[2]
    thresh <- c(threshL, threshR, threshL, threshR)
    
    res <- tibble(
      surgery = vlabs[s],
      var = vlabs[v],
      thresh = thresh,
      status = x_sim$status,
      side = rep(c("Left", "Right"), 2),
      y_value = y_pred,
      good_prob = round(100 * y_prob),
      method = "CART"
    )
  }
  
  # Get outcome for all variables ----
  res <- voutlist %>% 
    map(get_outcome_v1, out_list_CART=out_list_CART, x_sim=x_sim) %>% 
    bind_rows() 
  return(res)
}

# ---- NB: Predict outcomes for a given surgery s ----
pred_pt_outcome_NB <- function(xx=xpt, s, out_list_NB){
  # ---- Build control x ----
  x_control <- 
    xx %>% 
    mutate(
      across(
        starts_with("interval_"),
        ~ factor("0", levels = c("1", "0"))
      ),
      status = "Control",
    )
  
  # ---- Build treated x ----
  x_treated <- 
    x_control %>% 
    mutate(
      !!sym(glue("interval_{s}")) := factor("1", levels = c("1", "0")),
      status = "Treated"
    )
  x_sim <- bind_rows(x_treated, x_control)
  
  # ---- Get outcome variables ----
  voutlist <- get_outcome_vars(s)
  
  # ---- Get Treated and Control outcomes for one variable ----
  get_outcome_v1 <- function(out_list_NB, v, x_sim){
    # Choose model ----
    mod <- out_list_NB[[v]]$mod
    
    # Get outcome ----
    y_prob <- predict(mod, x_sim[names(mod$tables)], type = "prob")[,2]
    y_pred <- factor(y_prob >= .5, levels = c("FALSE", "TRUE"), labels=c("Poor","Good"))
    
    # Get threshold for v ----
    threshL <- get_outcome_thresh(v, x_sim[1,])
    threshL <- threshL[1]*threshL[2]
    threshR <- get_outcome_thresh(v, x_sim[2,])
    threshR <- threshR[1]*threshR[2]
    thresh <- c(threshL, threshR, threshL, threshR)
    
    res <- tibble(
      surgery = vlabs[s],
      var = vlabs[v],
      thresh = thresh,
      status = x_sim$status,
      side = rep(c("Left", "Right"), 2),
      y_value = y_pred,
      good_prob = round(100 * y_prob),
      method = "NB"
    )
  }
  
  # Get outcome for all variables ----
  res <- voutlist %>% 
    map(get_outcome_v1, out_list_NB=out_list_NB, x_sim=x_sim, .progress = TRUE) %>% 
    bind_rows() 
  return(res)
}

# ---- Format outcome table ----
fmt_checklist <- function(xx) {
  # Function to rename columns in final table -----
  rename_cols <- function(xx){
    old_names <- names(xx)
    new_names <- str_remove(old_names, "^(Left_|Right_)")
    new_names <- case_when(
      new_names == "surgery" ~ "Surgery",
      new_names == "blank" ~ "",
      new_names == "Structure" ~ "Structure",
      new_names == "Kinematic" ~ "Focal Kinematic",
      new_names == "GDI" ~ "Overall Kinematic",
      new_names == "FAQT" ~ "Mobility",
      new_names == "prop" ~ "Standard of Practice"
    )
    names(new_names) <- old_names
    return(new_names)
  } 
  # Choose pallette for bad/good ----
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  # Create summary table ----
  res <- 
    xx %>% 
    relocate(Left_prop, .after = surgery) %>%
    relocate(Right_prop, .after = blank) %>%
    gt() %>% 
    data_color(
      columns = matches("prop|Struct|Kinem|GDI|FAQT"),
      method = "bin",
      na_color = "grey90",
      domain = c(0, 100),
      bins = 5,
      palette = pal
    )  %>% 
    cols_width(
      surgery ~ "150px",
      blank ~ "15px",
      everything() ~ "80px"
    ) %>% 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_column_labels(columns = matches("prop|Struct|Kinem|GDI|FAQT|Left|Right")),
        cells_body(columns = matches("prop|Struct|Kinem|GDI|FAQT"))
      )
    ) %>% 
    fmt(
      columns = matches("prop|Struct|Kinem|GDI|FAQT"),
      fns = function(x) { 
        case_when(
          x < 20 ~ "<b>X</b>",
          x < 40 ~ "X",
          x < 60 ~ "<span style='color: #b3b3b3;'>~</span>",
          x < 80 ~ "\u2713",
          x < 999 ~ "<b>\u2713</b>"
        )
      }
    ) %>%
    tab_style(
      style = cell_borders(
        color = "#b3b3b3",
        weight = px(1)
      ),
      locations = cells_body(
        columns = matches("prop|Struct|Kinem|GDI|FAQT")
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>% 
    tab_source_note(
      source_note = md("\u2713 = Criterion met, X = Criterion not met, ~ = Uncertain<br>
                     **\u2713/X** = high confidence, \u2713/X = moderate confidence<br>
                       Overall Kinematic = Gait Deviation Index (GDI)<br> 
                       Mobilitiy = Functional Assessment Questionnaire Transform (FAQT)")
    ) %>%
    tab_options(
      column_labels.border.top.color = "transparent"
    ) %>% 
    cols_label(!!!rename_cols(xx)) %>% 
    tab_spanner(
      label = "Outcome",
      columns = c(3:6, 9:12),
      gather = FALSE
    ) %>%
    tab_spanner(
      label = "Treatment",
      columns = c(2, 8),
      gather = FALSE
    ) %>%
    tab_spanner(
      label = "Left",
      columns = c(2:6)
    ) %>% 
    tab_spanner(
      label = "Right",
      columns = c(8:12)
    ) %>% 
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_column_spanners(spanners = c("Left", "Right"))
    ) %>% 
    tab_options(
      table.font.size = px(12)
    ) %>% 
    tab_style(
      style = cell_borders(
        side = c("left", "right"),
        color = "black",
        weight = px(2)
      ),
      locations = cells_body(columns = matches("prop"))
    ) %>% 
    sub_missing(
      columns = everything(),
      missing_text = "Missing",
    )
  
  # Return formatted table ----
  return(res)
}

# ---- Organize outcomes Left and Right with blank in middle ----
organize_outcome <- function(xx, s, sd, st){
  xx <- 
    xx %>% 
    filter(
      surgery == vlabs[s],
      side == sd,
      status == st
    ) %>% 
    select(surgery, var, good_prob) %>% 
    mutate(var = c("Structure", "Kinematic", "GDI", "FAQT")) %>% 
    mutate(var = glue("{sd}_{var}")) %>% 
    pivot_wider(names_from = var, values_from = good_prob)
  return(xx)
}


# ---- Function to build and fomrat checlist summary tables ----
build_checklist_treated_control <- function(out_pt, ptprops){
  outL_treated <- 
    surglist %>% 
    map(organize_outcome, xx=out_pt, sd="Left", st="Treated") %>% 
    bind_rows() %>% 
    left_join(ptprops %>% filter(SIDE=="Left") %>% select(surgery=Surgery, Left_prop=q50))  
  outR_treated <- 
    surglist %>% 
    map(organize_outcome, xx=out_pt, sd="Right", st="Treated") %>% 
    bind_rows() %>% 
    left_join(ptprops %>% filter(SIDE=="Right") %>% select(surgery=Surgery, Right_prop=q50))  
  out_treated <- 
    bind_cols(outL_treated, outR_treated[,2:ncol(outR_treated)]) %>% 
    mutate(blank = "") %>% 
    relocate(blank, .before = Right_Structure)
  
  # ---- Get outcomes for left and right sides control ----
  outL_control <- 
    surglist %>% 
    map(organize_outcome, xx=out_pt, sd="Left", st="Control") %>% 
    bind_rows() %>% 
    left_join(ptprops %>% filter(SIDE=="Left") %>% select(surgery=Surgery, Left_prop=q50))
  outR_control <- 
    surglist %>% 
    map(organize_outcome, xx=out_pt, sd="Right", st="Control") %>% 
    bind_rows() %>% 
    left_join(ptprops %>% filter(SIDE=="Right") %>% select(surgery=Surgery, Right_prop=q50))
  out_control <- 
    bind_cols(outL_control, outR_control[,2:ncol(outR_control)]) %>% 
    mutate(blank = "") %>% 
    relocate(blank, .before = Right_Structure)
  
  checklist_treated <- fmt_checklist(out_treated)
  checklist_control <- fmt_checklist(out_control)
  
  return(list(checklist_treated, checklist_control))
}