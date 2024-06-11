gettables <- function() {
  # Open connection -----
  con <- odbcConnect("MotLabDB")
  tbllist <- sqlTables(con) %>% filter(TABLE_SCHEM == "dbo")

  # Get tables -----
  Diagnosis.tbl <- sqlFetch(con, "Diagnosis")
  DxMain.tbl <- sqlFetch(con, "_DXMain")
  DxMod1.tbl <- sqlFetch(con, "_DXMod1")
  DxMod2.tbl <- sqlFetch(con, "_DXMod2")
  Side.tbl <- sqlFetch(con, "_Side")

  Patient.tbl <- sqlFetch(con, "Patient")
  Patient_Events.tbl <- sqlFetch(con, "Patient_Events")
  Exam.tbl <- sqlFetch(con, "Exam")
  Services.tbl <- sqlFetch(con, "Services")
  ServiceType.tbl <- sqlFetch(con, "_Service_Type")
  Technician.tbl <- sqlFetch(con, "_Technician")
  Therapist.tbl <- sqlFetch(con, "_Therapist")
  PE.tbl <- sqlFetch(con, "Physical_Exam")
  SurgDefine.tbl <- sqlFetch(con, "SurgDefine")
  Surgical.tbl <- sqlFetch(con, "Surgical")
  SurgDetails.tbl <- sqlFetch(con, "Surgical_Details")
  SurgDetails_lookup.tbl <- sqlFetch(con, "_Detail")
  Treatment.tbl <- sqlFetch(con, "Treatment")
  Historical.tbl <- sqlFetch(con, "Historical")
  Referral.tbl <- sqlFetch(con, "Referral")

  TMH.tbl <- sqlFetch(con, "Trials_Motion_HDR")
  Trial_Type.tbl <- sqlFetch(con, "Trial_Type")
  Event.tbl <- sqlFetch(con, "Patient_Events")

  Oxygen.tbl <- sqlFetch(con, "Oxygen_Data")
  OxygenTrials.tbl <- sqlFetch(con, "Trials_Oxygen")

  GOAL.tbl <- sqlFetch(con, "GOAL_main")
  GOAL_items.tbl <- sqlFetch(con, "GOAL_Items")
  GOAL_imp.tbl <- sqlFetch(con, "_GOAL_Goal")
  GOAL_rating.tbl <- sqlFetch(con, "_GOAL_Rating")

  Func.tbl <- sqlFetch(con, "Functional")
  FuncFU.tbl <- sqlFetch(con, "FunctionalFU")

  Video.tbl <- sqlFetch(con, "Video")

  FMS_lookup <- sqlFetch(con, "_FMS")
  items_lookup.tbl <- sqlFetch(con, "_GOAL_Item")
  rating_lookup.tbl <- sqlFetch(con, "_GOAL_Rating")
  FuncRatePar_lookup <- sqlFetch(con, "_FuncRatePar")
  FunctionalFU_Activities_lookup <- sqlFetch(con, "_FunctionalFU_Activities")
  Effect_lookup <- sqlFetch(con, "_Effect")
  Outcome_lookup <- sqlFetch(con, "_Outcome")
  GMFCS_lookup <- sqlFetch(con, "_GMFCS")
  PtFreq_lookup <- sqlFetch(con, "_PtFreq")
  PtProg_lookup <- sqlFetch(con, "_PtProg")
  Lim_Fac_lookup <- sqlFetch(con, "_Lim_Fac")
  LimFactor_lookup <- sqlFetch(con, "_LimFactor")
  Delivery_lookup <- sqlFetch(con, "_Delivery")
  Orthotic_lookup <- sqlFetch(con, "_Device")
  DevType_lookup <- sqlFetch(con, "_DeviceType")
  Physician_lookup <- sqlFetch(con, "_Physician") %>%
    select(c(Physician_ID, Physician)) %>%
    rename(Ref_Physician_ID = Physician_ID)
  
  # Close connection
  close(con)
  
  listN <- function(...){
    anonList <- list(...)
    names(anonList) <- as.character(substitute(list(...)))[-1]
    anonList
  }

  return(
    listN(
      tbllist, Diagnosis.tbl, DxMain.tbl, DxMod1.tbl, DxMod2.tbl, Side.tbl,
      Patient.tbl, Patient_Events.tbl, Exam.tbl, Services.tbl, ServiceType.tbl,
      Technician.tbl, Therapist.tbl, PE.tbl, SurgDefine.tbl, Surgical.tbl,
      SurgDetails.tbl, SurgDetails_lookup.tbl, Treatment.tbl, Historical.tbl, 
      Referral.tbl,  TMH.tbl, Trial_Type.tbl, Event.tbl, Oxygen.tbl, OxygenTrials.tbl,
      GOAL.tbl, GOAL_items.tbl, Func.tbl, FuncFU.tbl, Video.tbl, FMS_lookup,
      items_lookup.tbl, FuncRatePar_lookup, FunctionalFU_Activities_lookup,
      Effect_lookup, Outcome_lookup, GMFCS_lookup, PtFreq_lookup, PtProg_lookup,
      Lim_Fac_lookup, LimFactor_lookup, Delivery_lookup, Orthotic_lookup, DevType_lookup,
      Physician_lookup, rating_lookup.tbl
    )
  )
}
