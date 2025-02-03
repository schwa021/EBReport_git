# isotree version -----
outlier_isoforest <- function(xpt, dat, surg){
  library(isotree)
  
  if(surg == "all"){
    # find all variables in all propensity models -----
    temp <- list()
    for (ss in surglist) {
      mod <- get(paste("mod_", ss, sep = ""))
      temp[[ss]] <- names(mod$X)
    }
    dvars <- temp %>% unlist() %>% unique() %>% str_subset("^prior|era|dx", negate = TRUE)
  } else if(surg == "dx") {
    dvars <- "dx"
  } else {
    # Find variables from specific model -----
    mod <- get(glue("mod_{surg}"))
    dvars <- str_subset(names(mod$X), "^prior|era|dx", negate = TRUE)
  }
  
  dd <- bind_rows(xpt, dat)
  
  df <- 
    dd %>% 
    select(all_of(dvars)) %>% 
    mutate(
      across(
        where(is.character),
        ~ as.factor(.)
      )
    )
  
  model <- isolation.forest(df, seed = 42)
  
  score <- predict(object=model, newdata=df)
  
  # For testing: find top outliers -----
  ix <- sort(score, decreasing = TRUE, index.return = TRUE)$ix[1:10]
  
  lab <- case_when(
    surg == "all" ~ "Overall",
    surg == "dx" ~ "Diagnosis",
    TRUE ~ str_replace(vlabs[glue("interval_{surg}")], "Interval ", "")
  )
  
  # hist(score, xlim=c(0.2,.8), breaks=15, main = lab, xlab = "Outlier Score \n Small = Common, Large = Rare")
  # abline(v = quantile(score, probs = 0.95), lty = 3, col="grey", lwd=2) # Dashed line for 5th percentile
  # abline(v = quantile(score, probs = 0.99), lty = 3, lwd=2) # Dashed line for 10th percentile
  # abline(v = score[1:2], col = c("red", "green2"), lwd=2)
  # phist <- recordPlot()
  
  # Find the patient quantile -----
  ecdf_d <- ecdf(score)
  q_pt <- ecdf_d(score[1:2])
  
  # plot(ecdf_d, col.01line = "white", xlab = "Median Length", ylab = "Quantile", main = "", bty = "none")
  # points(x = score[1:2], y = ecdf_d(score[1:2]), col = c("red", "green2"), cex=1.2, lwd=2, pch=19)
  # abline(h=.95, lty=2, col="grey")
  # abline(h=.99, lty=2, col="black")
  # lines(x = c(0, score[1]), y = c(q_pt[1], q_pt[1]), col = "red", lwd = .5, lty=2)
  # lines(x = c(0, score[2]), y = c(q_pt[2], q_pt[2]), col = "green2", lwd = .5, lty=2)
  # pcdf <- recordPlot()
  
  return(list(q_pt = round(100*q_pt)))
  
}
