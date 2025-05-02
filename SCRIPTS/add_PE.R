add_PE <- function(FD, PE.tbl){
  
  temp <- PE.tbl %>% 
    select(sort(names(.))) %>% 
    select(-c(ANKLE_DIAM_L_AFO, ANKLE_DIAM_R_AFO, starts_with("BEIGHTON"), contains("ELBOW"), contains("FOREARM"), contains("HAND"), contains("SHOULDER"), contains("STAND_POSTURE"), contains("THUMB_ABD"), contains("WRIST"), contains("THUMB"), ASIS_GT_DISTL, Physical_Exam_Type)) 
  
  FD <- FD %>% 
    left_join(temp)
  
  # Stack L and R sides (one row per side, rather than bilateral data)
  # Fixed variables
  FD_Fixed <- 
    FD %>% 
    select(!ends_with("_L") & !ends_with("_R"))
  
  # L and R Side variables
  FD_L <- 
    FD %>% 
    select(ends_with("_L"))
  
  FD_L$SIDE_DB <- "L"
  FD_L <- bind_cols(FD_Fixed, FD_L)
  names(FD_L) <- str_replace_all(names(FD_L), "_L$", "")
  
  FD_R <- FD %>% select(ends_with("_R"))
  FD_R$SIDE_DB <- "R"
  FD_R <- bind_cols(FD_Fixed, FD_R)
  names(FD_R) <- str_replace_all(names(FD_R), "_R$", "")
  
  FD <- bind_rows(FD_L, FD_R)
  
  temp <- c("BUN_DEF", "CONFUSION_TEST", "LIGAM_LAXITY", "OBER", "FIR_MTP_DF")
  FD <- FD %>%
    mutate(across(
      all_of(temp),
      ~ case_when(
        . == "U" ~ "Missing",
        . == "u" ~ "Missing",
        . == "y" ~ "Y",
        . == "n" ~ "N",
        . == "typ" ~ "TYP",
        . == "res" ~ "RES",
        . == "hyp" ~ "HYP",
        . == "-99" ~ "Missing",
        T ~ .
      ))) %>% 
    mutate(across(all_of(temp), ~ factor(., levels = c("N", "Y", "RES", "TYP", "HYP", "Missing"))))
  
  # Fix the PATELLA_ALTA variable - formerly had "res" and "hyp" levels (MHS 23/APR/2025)
  temp <- c("PATELLA_ALTA")
  FD <- FD %>%
    mutate(across(
      all_of(temp),
      ~ case_when(
        . == "U" ~ "Missing",
        . == "u" ~ "Missing",
        . == "y" ~ "Y",
        . == "n" ~ "N",
        . == "-99" ~ "Missing",
        T ~ .
      ))) %>% 
    mutate(across(all_of(temp), ~ factor(., levels = c("N", "Y", "Missing"))))
  
  # Add/fix SIDE
  FD <- FD %>%
    rename(SIDE = SIDE_DB) %>% 
    mutate(SIDE = factor(SIDE, levels = c("L", "R")))
  
  #Add MRN
  FD <- FD %>%
    mutate(MRN = trunc(Hosp_Num/10)) %>%
    relocate(MRN, .after = Hosp_Num)
  
  return(FD)
}