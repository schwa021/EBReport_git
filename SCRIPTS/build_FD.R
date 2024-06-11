build_FD <- function(tbls, mrn_in) {
  
  # Patient info
  temp <- tbls$Patient.tbl %>%
    select(sort(names(tbls$Patient.tbl))) %>%
    left_join(tbls$Patient_Events.tbl) %>%
    filter(Patient_Event_Type_ID == 2) %>%
    mutate(age = as.numeric(difftime(Event_Date, Birth_Date,
                                     unit = "days"
    ) / 365.25)) %>%
    mutate(Sex = ifelse(Sex == "U", NA, Sex)) %>%
    mutate(Sex = factor(Sex, levels = c("F", "M"))) %>%
    mutate(Sex = fct_na_value_to_level(Sex, level = "Missing")) %>%
    select(-c(
      CPI_Number, starts_with("Address"), City, State,
      Phone_Num, Archived, Event_Time, Birth_Date
    )) %>%
    select(-c(Patient_Event_Type_ID)) |> 
    mutate(MRN = trunc(Hosp_Num/10))
  
  
  # Case or Full Dataset
    FD <- 
      temp %>%
      filter(MRN == mrn_in)

  
  # Exam and Services info
  temp <- tbls$Exam.tbl %>%
    select(sort(names(tbls$Exam.tbl))) %>%
    select(-c(DxCharges_ID, SHCC_PO_NUM)) %>%
    left_join(tbls$Services.tbl) %>%
    filter(Service_Type_ID == 2) %>%
    left_join(tbls$Technician.tbl) %>%
    left_join(tbls$Therapist.tbl, by = "Therapist_ID") %>%
    select(-c(Technician_ID, Therapist_ID, IsUnknown.x, IsUnknown.y, IsCommon.x, IsCommon.y, Service_Type_ID)) %>%
    mutate(across(.cols = c(Technician, Therapist), ~ as.factor(.))) %>%
    mutate(across(.cols = c(Technician, Therapist), ~ fct_na_value_to_level(., level = "Missing")))
  
  # Join
  FD <- 
    FD %>%
    left_join(temp) %>%
    drop_na(Exam_ID)
  
  return(FD)
}