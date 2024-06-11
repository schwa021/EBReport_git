add_EOS <- function(FD, con2) {
  
  # Get EOS table
  EOS <- sqlFetch(con2, "rpt.vw_Gait_Lab_EOS_Results_and_Rad_Order_Lower_Limb")
  close(con2)

  EOSlong <- EOS %>%
    rename(EOS_LegLength_L = Anatomical_Length_Left) %>%
    rename(EOS_LegLength_R = Anatomical_Length_Right) %>%
    rename(EOS_FemTor_L = Femoral_Torsion_Left) %>%
    rename(EOS_FemTor_R = Femoral_Torsion_Right) %>%
    mutate(EOS_Bimal_L = Femorotibial_Rotation_Left + Tibial_Torsion_Left) %>%
    mutate(EOS_Bimal_R = Femorotibial_Rotation_Right + Tibial_Torsion_Right) %>%
    rename(EOS_MRN = Pat_MRN) %>%
    rename(EOS_Visit_Date = Acquisition_date) %>%
    select(colnames(.) %>% str_subset("EOS_")) %>%
    select(colnames(.) %>% str_subset("EOS_Model", negate = TRUE)) %>%
    pivot_longer(
      cols = -c("EOS_MRN", "EOS_Visit_Date"),
      names_to = c(".value", "EOS_Side"),
      names_pattern = "(.+)_(.+$)",
      values_to = ".names",
      values_drop_na = TRUE
    )


  FD <- left_join(FD, EOSlong, by = c("MRN" = "EOS_MRN", "SIDE" = "EOS_Side")) %>%
    mutate(EOS_date_diff = as.numeric(difftime(Event_Date, EOS_Visit_Date, units = "days"))) %>%
    filter(is.na(EOS_date_diff) | abs(EOS_date_diff) < 180) %>%
    select(c("Exam_ID", "SIDE", "EOS_date_diff", colnames(.) %>% str_subset("EOS_"))) %>%
    distinct() %>%
    right_join(FD, by = join_by(Exam_ID, SIDE))

  # If multiple EOS dates, choose the last one prior to (or on date of) gait visit
  FD <-
    FD %>%
    group_by(Exam_ID) %>%
    filter(EOS_date_diff == max(EOS_date_diff, na.rm = T) | is.na(EOS_date_diff)) %>%
    ungroup()

  return(FD)
}