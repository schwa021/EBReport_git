add_Referral <- function(FD, Referral.tbl) {
  # Referral
  temp <- Referral.tbl %>%
    select(sort(names(Referral.tbl))) %>%
    select(-c(Ambulation_ID, Behavior_Type_ID, Patient_Type_ID, Ref_Comments, Study_Association_ID, RefSource_ID))


  FD <- FD %>%
    left_join(temp)

  return(FD)
}