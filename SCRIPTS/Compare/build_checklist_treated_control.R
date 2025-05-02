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