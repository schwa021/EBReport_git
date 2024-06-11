add_Oxygen <- function(FD, Exam.tbl, Services.tbl, OxygenTrials.tbl, 
                       Oxygen.tbl, Orthotic_lookup){
  
  summarize <- dplyr::summarize
  
  temp <- Exam.tbl %>% 
    left_join(Services.tbl) %>% 
    left_join(OxygenTrials.tbl) %>% 
    left_join(Oxygen.tbl) %>% 
    drop_na(VELOC) %>% 
    filter(VELOC > 0) %>% 
    mutate(across(everything(), ~ ifelse(. == -99, NA, .)))
  
  Oxy <- temp %>% 
    mutate(across(everything(), ~ ifelse(. == -99, NA, .))) %>% 
    select(Exam_ID, contains("Device_ID"), Oxygen_Data_ID, VELOC, STR_LENGTH, OXY_CONS, OXY_COST, SIT_OXY_CONS, STAND_OXY_CONS, PCT_O2_CONSUMP, OrthDev_L_Device_ID, OrthDev_R_Device_ID, AssistDev_L_Device_ID, AssistDev_R_Device_ID, Filename, RAW_VELOC, RAW_GROSS_OXY_CONS, RAW_SIT_OXY_CONS) %>% 
    select(-contains("Prosth")) %>% 
    mutate(STEP_LENGTH = STR_LENGTH/2) %>% 
    rename(O2_NDspeed = VELOC, 
           O2_NDsteplen = STEP_LENGTH, 
           NETND_OXYCONS = OXY_CONS, 
           NETND_OXYCOST = OXY_COST, 
           ND_SITCONS = SIT_OXY_CONS, 
           ND_STANDCONS = STAND_OXY_CONS) %>% 
    select(-STR_LENGTH) %>% 
    left_join(Orthotic_lookup, by = c("OrthDev_L_Device_ID" = "Device_ID")) %>% 
    rename(O2_L_Orthosis = Device) %>% 
    left_join(Orthotic_lookup, by = c("OrthDev_R_Device_ID" = "Device_ID")) %>% 
    rename(O2_R_Orthosis = Device) %>% 
    left_join(Orthotic_lookup, by = c("AssistDev_L_Device_ID" = "Device_ID")) %>% 
    rename(O2_L_Assistive = Device) %>% 
    left_join(Orthotic_lookup, by = c("AssistDev_R_Device_ID" = "Device_ID")) %>% 
    rename(O2_R_Assistive = Device) %>% 
    select(-c(ends_with(".x"), ends_with(".y"), contains("Dev_"))) %>% 
    relocate(O2_NDsteplen, .after = O2_NDspeed)
  
  # Clean up
  temp <- sapply(Oxy, function(x) is.numeric(x) | is.integer(x))
  nums = names(temp[temp])
  Oxy <- Oxy %>% 
    filter(NETND_OXYCONS > 0,
           O2_NDspeed < 0.8,
           O2_NDsteplen < .7 | is.na(O2_NDsteplen)) %>% 
    mutate(across(all_of(names(nums)), ~ ifelse(. == -99, NA, .)))
  
  
  # Keep First Barefoot if multiple BF tests exist, or first test if not
  temp <- Oxy %>% group_by(Exam_ID) %>% summarize(nex = n())
  exams <- temp[["Exam_ID"]][temp$nex > 1]
  
  for (ex in exams) {
    # extract data with more than 1 row per exam (multiple O2 tests)
    dat <- Oxy %>% filter(Exam_ID == ex)
    
    # Initialize which trial to keep (all False)
    Lkeep <- rep(F, nrow(dat))
    Rkeep <- rep(F, nrow(dat))
    keep <- rep(F, nrow(dat))
    
    # Find all Unk/None conditions
    Lbf <- dat$O2_L_Orthosis %in% c("Unknown", "None")
    Rbf <- dat$O2_R_Orthosis %in% c("Unknown", "None")
    
    # Look for Unk/None and keep first found
    if(sum(Lbf)==0) Lkeep[1] <- T
    if(sum(Rbf)==0) Rkeep[1] <- T
    if(sum(Lbf)>0) Lkeep[first(which(Lbf))] <- T
    if(sum(Rbf)>0) Rkeep[first(which(Rbf))] <- T
    
    # if one found, keep it, 
    ikeep <- ifelse(sum(Lkeep & Rkeep) > 0, first(which(Lkeep)), 1)
    keep[ikeep] <- T
    
    dat[!keep,] <- NA
    
    Oxy[which(Oxy$Exam_ID == ex),] <- dat
  }
  
  Oxy <- Oxy[rowSums(is.na(Oxy)) != ncol(Oxy), ]
  
  FD <- FD %>% 
    left_join(Oxy)
  
  return(FD)
  
}