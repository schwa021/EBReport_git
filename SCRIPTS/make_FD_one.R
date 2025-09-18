make_FD_one <- function(datc3d, mrn_in){
  # library(data.table)
  library(tidyverse)
  library(lubridate)
  library(RODBC)
  library(glue)
  
  table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)
  
  # Get tables from databse -----
  source("SCRIPTS/gettables.R")
  tbls <- gettables()
  
  # Patient and Exam -----
  source("SCRIPTS/build_FD.R")
  FD <- build_FD(tbls, mrn_in)
  
  # PE -----
  source("SCRIPTS/add_PE.R")
  FD <- add_PE(FD, tbls$PE.tbl)
  
  # EOS Data -----
  source("SCRIPTS/add_EOS.R")
  # con2 <- odbcConnect("Azure_Connection", uid = "mschwartz@gillettechildrens.com", pwd = "Duffy001!))))))))")
  con2 <- odbcConnect("Azure_Connection", uid = params$username, pwd = params$password)
  # Check for working con2 - this avoids crashing if EOS database is down
  if(con2 != -1)  FD <- add_EOS(FD, con2)
  
  # Historical -----
  source("SCRIPTS/add_Historical.R")
  FD <- add_Historical(FD, tbls$Historical.tbl, tbls$Delivery_lookup)
  
  # Function (now includes FAQT)
  source("SCRIPTS/add_Function.R")
  FD <- add_Function(FD, tbls)
  
  # Referral
  source("SCRIPTS/add_Referral.R")
  FD <- add_Referral(FD, tbls$Referral.tbl)
  
  # Build Diagnosis data (Dx)
  source("SCRIPTS/add_Diagnosis.R")
  FD <- add_Diagnosis(FD, tbls$Diagnosis.tbl, tbls$DxMain.tbl, tbls$DxMod1.tbl, 
                      tbls$Side.tbl)
  
  # Oxygen
  source("SCRIPTS/add_Oxygen.R")
  FD <- add_Oxygen(FD, tbls$Exam.tbl, tbls$Services.tbl, tbls$OxygenTrials.tbl, 
                   tbls$Oxygen.tbl, tbls$Orthotic_lookup)
  
  # Build GOAL data
  source("SCRIPTS/add_GOAL.R")
  FD <- add_GOAL(FD, tbls$items_lookup.tbl, tbls$rating_lookup.tbl, 
                 tbls$GOAL_items.tbl, tbls$GOAL.tbl)
  
  # Build DMC and GDI data and add Physician
  source("SCRIPTS/add_Indexes.R")
  FD <- add_Indexes(FD, tbls$Patient.tbl, tbls$Patient_Events.tbl, tbls$Exam.tbl, 
                    tbls$Services.tbl, tbls$TMH.tbl, tbls$Trial_Type.tbl,
                    tbls$Physician_lookup)
  
  
  # Add video filename -----
  Videos <- tbls$Patient.tbl %>%
    left_join(tbls$Event.tbl) %>% 
    left_join(tbls$Exam.tbl) %>% 
    left_join(tbls$Services.tbl) %>% 
    left_join(tbls$Video.tbl) %>% 
    select(Hosp_Num, Event_Date, Exam_ID, Video_File) %>% 
    drop_na(Exam_ID, Video_File)
  
  FD <- FD %>% 
    left_join(Videos)
  
  
  # Organize FD -----
  FD <- 
    FD %>% 
    relocate(Exam_ID, Hosp_Num, SIDE, age, dx, dxmod, Sex, Zip, Event_Date, 
             Patient_ID, Patient_Event_ID, Service_ID, Physical_Exam_ID, 
             Historical_ID, Functional_ID, Functional_FU_ID, Diag_ID, Referral_ID, 
             Therapist, Technician) %>% 
    arrange(Hosp_Num, Exam_ID, SIDE)
  
  
  ## Identify Affected Side -----
  nobs <- nrow(FD)
  bi <- grepl("Diplegia", FD$dxmod) | grepl("Triplegia", FD$dxmod) | 
    grepl("Quadriplegia", FD$dxmod)
  uni <- !bi
  dxL <- grepl("L",FD$dxside, ignore.case=FALSE) | grepl("Bilateral", FD$dxside)
  dxR <- grepl("R",FD$dxside, ignore.case=FALSE) | grepl("Bilateral", FD$dxside)
  FD$affected <- bi | (dxL & FD$SIDE== "L") | (dxR & FD$SIDE == "R")
  
  # Clean up -----
  source("SCRIPTS/cleanup_FD.R")
  FD <- cleanup_FD(FD)
  
  # Make and/or Label Factors any _ID as factor -----
  IDlist <- str_subset(names(FD), "ID$")
  FD <- 
    FD %>% 
    mutate(across(all_of(IDlist), ~ factor(.))) %>% 
    mutate(Hosp_Num = factor(Hosp_Num))
  
  # Only keep LR exams -----
  source("SCRIPTS/keep_LRexams.R")
  FD <- keep_LRexams(FD)
  
  # Build surgery table -----
  source("SCRIPTS/build_surg.R")
  surg <- build_surg(tbls)
  
  # Get Prior Surgery -----
  source("SCRIPTS/add_prior_surgery.R")
  FD <- add_prior_surgery(FD, surg)
  
  # Extract Average Gait Statistics -----
  if(!is.null(datc3d)){
    source("SCRIPTS/add_stats.R")
    FD <- add_stats(FD, datc3d)
  }
  
  # Add Gait Features -----
  if(!is.null(datc3d)){
    source("SCRIPTS/add_features.R")
    FD <- add_features(FD, datc3d)
  }
  
  # Fix names -----
  FD <- 
    FD %>% 
    tibble(.name_repair = "universal") %>% 
    data.frame()
  names(FD) <- str_replace_all(names(FD), "[.]", "_")
  names(FD) <- str_replace_all(names(FD), "___", "_")
  
  # Fix -99s -----
  temp <- sapply(FD, function(x) is.numeric(x) | is.integer(x))
  nums = names(temp[temp])
  FD <- 
    FD %>% 
    mutate(across(all_of(nums), function(x) ifelse(x == -99, NA, x)))
  
  # Assign Attributes (e.g., nicename, units) -----
  # TODO
  
  # Add speed, steplength, cadence -----
  if(!is.null(datc3d)){
    source("SCRIPTS/add_strideparams.R")
    FD <- add_strideparams(FD, datc3d)
  }
  
  # Compute GMFCS ----
  if(!is.null(datc3d)){
    source("SCRIPTS/compute_GMFCS.R")
    FD <- compute_GMFCS(FD)
  } else {
    FD$GMFCS_meas <- FD$GMFCS
    FD$GMFCS_computed <- FD$GMFCS
  }
  
  
  ########################################
  ##      LOOK FOR CANCELLED APPT       ##
  ########################################
  # If there is not video file, appt was cancelled
  # TODO: use explicit appt. status
  ix <- !is.na(FD$Video_File)
  FD <- FD[ix, ]
  
  
  ##########################################
  ##        GENERATE PRE-POST VERSION     ##
  ##########################################
  
  # Generate Pre-Post version
  source("SCRIPTS/build_prepost.R")
  FDpp <- build_prepost(FD)
  
  # Get interval treatment
  source("SCRIPTS/add_interval_treatment.R")
  FDpp <- add_interval_treatment(FDpp)
  
  # Get surgical details for FDpp
  source("SCRIPTS/add_interval_details.R")
  FDpp <- add_interval_details(FDpp, surg)
  
  
  return(list(FD = FD, FDpp = FDpp))
  
}
