
# Predict features scores from pre-build models --------------------------------
predict_scores <- function(d, exam, side, mod, nsim, pp){
  
  exlhs <- ifelse(pp == "Post", "Exam_IDPost", "Exam_ID")
  
  # Get data for subject -----
  xnew <- 
    d %>% 
    filter(
      .data[[exlhs]] == {{exam}},
      SIDE ==  {{side}}
    ) %>% 
    select(names(mod$X))
  
  # Compute nsim posteior prediction scores -----
  score <- calc_prediction_intervals(mod, xnew, num_samples_per_data_point = nsim)$all_prediction_samples
  
  return(score)
}