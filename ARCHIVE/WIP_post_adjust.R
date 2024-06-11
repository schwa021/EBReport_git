# Perform post-matching adjustment to outcome. This accounts for the range of
# features in the matches, and that the outcome variable can vary with these features

del_adjust_bart <- function(vout, temp, modvars){
  voutPost <- glue("{vout}Post")
  del <- temp[[voutPost]] - temp[[vout]]
  temp$del <- del
  temp <- temp %>% drop_na(del)
  M <- temp %>% select(all_of(modvars), del) %>% drop_na(del)
  
  set.seed(42)
  xxx <- M %>% select(all_of(modvars)) %>% data.frame()
  yyy <- M %>% pull(del)
  del_adj_mod <- bartMachine(xxx, yyy, use_missing_data = T, seed = 42)
  zzz <- calc_prediction_intervals(del_adj_mod, xside[names(del_adj_mod$X)], pi_conf = .90)
  
  return(zzz$all_prediction_samples)
}

del_pred_bart <- function(vout, dat, modvars, surglist){
  mfeat <- c(modvars, glue("interval_{surglist}"), "age", "GMFCS")
  voutPost <- glue("{vout}Post")
  M <- 
    dat %>% 
    mutate(del = dat[[voutPost]] - dat[[vout]]) %>% 
    select(all_of(mfeat), del) %>% 
    drop_na(del)
  
  set.seed(42)
  xxx <- M %>% select(all_of(mfeat)) %>% data.frame()
  yyy <- M %>% pull(del)
  del_pred_mod <- bartMachine(xxx, yyy, use_missing_data = T, seed = 42)
  
  return(del_pred_mod)
}

# Pick surgery and outcome -----
s <- "Psoas_Release"
vout <- "ANTEVERSION"

# Get matched data for surgery s -----
temp0 <- pmatch_data(s, datx=dat, xside)$dmatch0
temp1 <- pmatch_data(s, datx=dat, xside)$dmatch1

# Raw matching -----
zzz1 <- temp1[[glue("{vout}Post")]] - temp1[[vout]]
zzz0 <- temp0[[glue("{vout}Post")]] - temp0[[vout]]
pzrawmatch <- tibble("post" = c(zzz0, zzz1), status = c(rep("control", length(zzz1)), rep("treated", length(zzz0))))
pzrawmatch$model <- "Raw Match"

# ggplot(pzrawmatch, aes(x=post, color=status, fill=status)) +
#   geom_density(linewidth = 1, alpha = .1) +
#   facet_wrap(~status, ncol=1) +
#   scale_color_discrete_diverging(palette = "Red-Green") +
#   scale_fill_discrete_diverging(palette = "Red-Green")


# Adjusted matching -----
match_vars <- get_matching_vars(s)
zzz1 <- del_adjust_bart(vout, temp1, match_vars)
zzz0 <- del_adjust_bart(vout, temp0, match_vars)
pzadjmatch <- tibble("post" = c(zzz0[1,], zzz1[1,]), status = c(rep("control", ncol(zzz1)), rep("treated", ncol(zzz0))))
pzadjmatch$model <- "Adj. Match"

# ggplot(pzadjmatch, aes(x=post, color=status, fill=status)) +
#   geom_density(linewidth = 1, alpha = .1) +
#   facet_wrap(~status, ncol=1) +
#   scale_color_discrete_diverging(palette = "Red-Green") +
#   scale_fill_discrete_diverging(palette = "Red-Green")


# BART -----
# Build model
surg <- glue("interval_{s}")
xpred0 <- xside
xpred0[ , glue("interval_{surglist}")] <- factor(0, levels = c(1, 0))
xpred1 <- xpred0
xpred1[ , surg] <- factor(1, levels = c(1, 0))
del_pred_mod <- del_pred_bart(vout, dat, get_matching_vars(s), surglist)

# Make Prediction
zzz1 <- calc_prediction_intervals(del_pred_mod, xpred1[names(del_pred_mod$X)])$all_prediction_samples
zzz0 <- calc_prediction_intervals(del_pred_mod, xpred0[names(del_pred_mod$X)])$all_prediction_samples
pzbart <- tibble("post" = c(zzz0[1,], zzz1[1,]), status = c(rep("control", ncol(zzz1)), rep("treated", ncol(zzz0))))
pzbart$model <- "BART"

# ggplot(pzbart, aes(x=post, color=status, fill=status)) + 
#   geom_density(linewidth = 1, alpha = .1) + 
#   facet_wrap(~status, ncol=1) + 
#   scale_color_discrete_diverging(palette = "Red-Green") +
#   scale_fill_discrete_diverging(palette = "Red-Green")


# Compare -----
pz <- bind_rows(pzbart, pzadjmatch, pzrawmatch)

ggplot(pz, aes(x=post, color=status, fill=status)) + 
  geom_density(linewidth = 1, alpha = .1) + 
  geom_vline(xintercept = 0, color = "grey30") +
  facet_wrap(~model+status, ncol=2) + 
  scale_color_discrete_diverging(palette = "Red-Green") +
  scale_fill_discrete_diverging(palette = "Red-Green") +
  labs(
    title = glue("\u0394 {vout} following {s}")
  )
