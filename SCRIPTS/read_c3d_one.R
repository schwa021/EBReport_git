# This is the main function for reading data from a c3d file and organizing it -----

read_c3d_one <- function(mrn_in, tt){
  # Load Libraries -----
  library(tidyverse)
  library(glue)
  library(DBI)
  
  # Load functions -----
  # Main C3D Reader
  source("SCRIPTS/ReadC3D.R")
  # Parameter Reader
  source("SCRIPTS/ReadC3DParameters.R")
  # Function to make 51 point spline
  source("SCRIPTS/makespline.R")
  # Function to extract data from single stride
  source("SCRIPTS/get_cycle_data.R")
  # Function to find events for all complete strides bilaterally
  source("SCRIPTS/find_all_strides.R")
  # Function to convert events from frame number to pct gait cycle
  source("SCRIPTS/cyclepct.R")
  # Function to convert raw c3d data names to nice names
  source("SCRIPTS/nicenames.R")
  # Function to extract data (angle, moment, power, reaction)
  source("SCRIPTS/extract_data.R")
  # Function to convert to long format
  source("SCRIPTS/make_long.R")
  # Function to read and format data from GCD files
  source("SCRIPTS/ReadGCD.R")
  # Function to get list of files
  source("SCRIPTS/get_file_list.R")
  # Function to stretch all sub-phases of cycle to 50 points each
  source("SCRIPTS/stretchwrap.R")
  # Function to get statistic from stretched data
  source("SCRIPTS/getstat.R")
  # Get Statistics -----
  source("SCRIPTS/allstats.R")
  
  # Choose all existing files: note BF vs. AFO via "tt" -----
  xlist_new <- 
    get_file_list() |> 
    filter(
      MRN == mrn_in,
      TrialType_ID == tt
    )
  
  if(nrow(xlist_new) == 0){
    retlist <- NULL
    return(retlist)
  }
  
  # Build TrialInfo -----
  TrialInfo <-
    xlist_new %>%
    select(
      Patient_ID, MRN, Exam_ID, Trial_Num, TrialType_ID, Event_Date, WEIGHT,
      LEG_LENGTH_L, LEG_LENGTH_R, Sex, Birth_Date, Filename, Performed_Kinematics,
      Performed_Kinetics, matches("AssistDev|OrthDev")
    ) %>%
    mutate(
      across(starts_with("LEG_LEN"), ~ . / 100),
      Event_Date = as.Date(Event_Date),
      Age = as.numeric(difftime(Event_Date, Birth_Date, units = "days")) / 365.25,
      t_extracted = as.POSIXct(0, origin = "1900-01-01")
    ) %>% 
    select(-Birth_Date) |>
    mutate(needupdate = T)
  
  # Allocate Ang/Mom/Pwr/... for trial values -----
  Ang <- vector(mode = "list", length = nrow(TrialInfo))
  Mom <- vector(mode = "list", length = nrow(TrialInfo))
  Pwr <- vector(mode = "list", length = nrow(TrialInfo))
  Trk <- vector(mode = "list", length = nrow(TrialInfo))
  Rxn <- vector(mode = "list", length = nrow(TrialInfo))
  Len <- vector(mode = "list", length = nrow(TrialInfo))
  ixupdate <- which(rep(TRUE, nrow(TrialInfo)))
  
  # Loop over  files in list -----
  for (kk in 1:max(ixupdate)) {

    # Read in c3d file -----
    # If there are no kinematics then skip file
    if(!TrialInfo$Performed_Kinematics[kk]) next
    
    # If it's a C3D file -----
    if(str_detect(TrialInfo$Filename[kk], "c3d|C3d|C3D")){ 
      
      # Read in C3D file using Bruce MacWilliams' ReadC3D function
      # Temp fix after IS fucked us over with path name change (str_replace)
      temppath <- str_replace(TrialInfo$Filename[kk], "faplocal", "gcfiles")
      c <- tryCatch(
        ReadC3D(C3DFileName = temppath, MarkerDataFormat = "wide"),
        error = function(e) NULL
      )
      if(is.null(c) | is.null(c$Header)) next
      
      # Extract Angles (Ang), Trunk data (Trk), etc... -----
      Ang[[kk]] <- extract_data(c, typestr = "Angle")
      Trk[[kk]] <- extract_data(c, typestr = "Trunk")
      Len[[kk]] <- extract_data(c, typestr = "Length")
      if(TrialInfo$Performed_Kinetics[kk] == 1){
        Mom[[kk]] <- extract_data(c, typestr = "Moment")
        Pwr[[kk]] <- extract_data(c, typestr = "Power")
        Rxn[[kk]] <- extract_data(c, typestr = "Reaction")
      }
      
      # If there is data then reshape to long format -----
      if(nrow(Ang[[kk]]) > 0) Ang[[kk]] <- 
        make_long(Ang[[kk]], kk, "Angle", TrialInfo)
      
      if(nrow(Trk[[kk]]) > 0) Trk[[kk]] <- 
        make_long(Trk[[kk]], kk, "Trunk", TrialInfo)
      
      if(nrow(Len[[kk]]) > 0) Len[[kk]] <- 
        make_long(Len[[kk]], kk, "Length", TrialInfo)
      
      if(TrialInfo$Performed_Kinetics[kk] == 1){
        Mom[[kk]] <- make_long(Mom[[kk]], kk, "Moment", TrialInfo)
        Pwr[[kk]] <- make_long(Pwr[[kk]], kk, "Power", TrialInfo)
        Rxn[[kk]] <- make_long(Rxn[[kk]], kk, "Reaction", TrialInfo)
      }
      
    }  else {
      # If file is a GCD file we jump to here
      g <- ReadGCD(TrialInfo, kk)
      Ang[[kk]] <- g$ang
      Mom[[kk]] <- g$mom
      Pwr[[kk]] <- g$pwr
      Rxn[[kk]] <- g$rxn
      Trk[[kk]] <- tibble()
      Len[[kk]] <- tibble()
  }
    # Update time extracted -----
    TrialInfo$t_extracted[kk] <- now()
}   # End of file reading loop
  
  # Name the list elements by patientid, examid, and trial -----
  names(Ang) <- glue("MRN.{mrn_in}.Exam_ID.{TrialInfo$Exam_ID}.Trial.{TrialInfo$Trial_Num}")
  names(Mom) <- glue("MRN.{mrn_in}.Exam_ID.{TrialInfo$Exam_ID}.Trial.{TrialInfo$Trial_Num}")
  names(Pwr) <- glue("MRN.{mrn_in}.Exam_ID.{TrialInfo$Exam_ID}.Trial.{TrialInfo$Trial_Num}")
  names(Rxn) <- glue("MRN.{mrn_in}.Exam_ID.{TrialInfo$Exam_ID}.Trial.{TrialInfo$Trial_Num}")
  names(Trk) <- glue("MRN.{mrn_in}.Exam_ID.{TrialInfo$Exam_ID}.Trial.{TrialInfo$Trial_Num}")
  names(Len) <- glue("MRN.{mrn_in}.Exam_ID.{TrialInfo$Exam_ID}.Trial.{TrialInfo$Trial_Num}")
  
  # Compute Exam Averages -----  
  source("SCRIPTS/examavg.R")
  exlist <- unique(TrialInfo$Exam_ID)
  Ang_avg <- map(.x = exlist, ~ examavg(D = Ang, TrialInfo, id = .x))
  Mom_avg <- map(.x = exlist, ~ examavg(D = Mom, TrialInfo, id = .x))
  Pwr_avg <- map(.x = exlist, ~ examavg(D = Pwr, TrialInfo, id = .x))
  Rxn_avg <- map(.x = exlist, ~ examavg(D = Rxn, TrialInfo, id = .x))
  Trk_avg <- map(.x = exlist, ~ examavg(D = Trk, TrialInfo, id = .x))
  Len_avg <- map(.x = exlist, ~ examavg(D = Len, TrialInfo, id = .x))
  
  # Name the list elements by patientid, examid, and trial -----
  names(Ang_avg) <- glue("MRN.{mrn_in}.Exam_ID.{exlist}")
  names(Mom_avg) <- glue("MRN.{mrn_in}.Exam_ID.{exlist}")
  names(Pwr_avg) <- glue("MRN.{mrn_in}.Exam_ID.{exlist}")
  names(Rxn_avg) <- glue("MRN.{mrn_in}.Exam_ID.{exlist}")
  names(Trk_avg) <- glue("MRN.{mrn_in}.Exam_ID.{exlist}")
  names(Len_avg) <- glue("MRN.{mrn_in}.Exam_ID.{exlist}")
  
  # Compute statistics -----
  # Stretchwrap every new Exam_ID -----
  # This is done to ensure that all sub-phases of the gait cycle have the same number of points
  # Which, in turn, allows us to vectorize the statistics calculations
  samp <- 1:length(Ang_avg)
  angdat <- map(.x = samp, ~ stretchwrap(Ang_avg[[.x]], d = 0)) %>% list_cbind()
  momdat <- map(.x = samp, ~ stretchwrap(Mom_avg[[.x]], d = 0)) %>% list_cbind()
  pwrdat <- map(.x = samp, ~ stretchwrap(Pwr_avg[[.x]], d = 0)) %>% list_cbind()
  rxndat <- map(.x = samp, ~ stretchwrap(Rxn_avg[[.x]], d = 0)) %>% list_cbind()
  lendat <- map(.x = samp, ~ stretchwrap(Len_avg[[.x]], d = 0)) %>% list_cbind()
  trkdat <- map(.x = samp, ~ stretchwrap(Trk_avg[[.x]], d = 0)) %>% list_cbind()
  # Get lengthening rate (so-called "velocity") Note d = 1 takes analytic spline derivative -----
  veldat <- map(.x = samp, ~ stretchwrap(Len_avg[[.x]], d = 1)) %>% list_cbind()

  # Compute statistics -----
  Ang_stats <- allstats(angdat)
  Mom_stats <- allstats(momdat)
  Pwr_stats <- allstats(pwrdat)
  Rxn_stats <- allstats(rxndat)
  Trk_stats <- allstats(trkdat)
  Len_stats <- allstats(lendat)
  Vel_stats <- allstats(veldat)

  # Lengthening rate ("vel") stats need to be scaled by steplength/speed -----
  if(!is.null(Len_stats)){
  scaledat <-
    Len_avg %>%
    list_rbind() %>%
    select(side, Exam_ID, speed, steplen) %>%
    distinct() %>%
    mutate(vscale = steplen / speed,
           Exam_ID = as.character(Exam_ID)) %>%
    select(-c(speed, steplen))

  Vel_stats <-
    Vel_stats %>%
    left_join(scaledat) %>%
    mutate(value = value * vscale) %>%
    select(-vscale)
  } else {
    Vel_stats <- NULL
  }
  
  # Function to automatically name list elements -----
  listN <- function(...){
    anonList <- list(...)
    names(anonList) <- as.character(substitute(list(...)))[-1]
    anonList
  }
  
  retlist <- listN(
    Ang, Mom, Pwr, Rxn, Trk, Len,
    Ang_avg, Mom_avg, Pwr_avg, Rxn_avg, Trk_avg, Len_avg,
    Ang_stats, Mom_stats, Pwr_stats, Rxn_stats, Trk_stats, Len_stats, Vel_stats
  )
  
  return(retlist)
  }
