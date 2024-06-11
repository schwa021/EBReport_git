add_Diagnosis <- function(FD, Diagnosis.tbl, DxMain.tbl, DxMod1.tbl, Side.tbl) {
  # Diagnosis
  Dx <- Diagnosis.tbl %>%
    left_join(DxMain.tbl) %>%
    left_join(DxMod1.tbl) %>%
    left_join(Side.tbl) %>%
    filter(Diag_Level == 1) %>%
    rename(dxside = Side) %>%
    rename(dx = Dx) %>%
    rename(dxmod = DxMod1) %>%
    select(-c(Diag_Level, Dx_ID, DxMod1_ID, DxMod2_ID, Side_ID, Side_Code, Device_Valid, IsUnknown)) %>%
    mutate(across(c("dx", "dxmod", "dxside"), ~ factor(.))) %>%
    mutate(across(
      c("dx", "dxmod", "dxside"),
      ~ fct_na_value_to_level(., level = "Missing")
    ))

  Dx <- Dx %>%
    select(sort(names(Dx)))

  FD <- FD %>%
    left_join(Dx)

  return(FD)
}
