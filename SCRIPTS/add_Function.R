add_Function <- function(FD, tbls){
  
  temp <- tbls$Func.tbl
  
  # Recode FMS
  lookup <- tbls$FMS_lookup %>% 
    mutate(FMS_Details = str_replace(FMS_Details, "Unknown", "Missing")) %>% 
    select(-IsUnknown)
  
  temp <- temp %>% 
    left_join(lookup, by = c("FMS_5_ID" = "FMS_ID")) %>% 
    select(-c(FMS_5_ID, FMS_Desc)) %>% 
    rename(FMS_5 = FMS_Details) %>% 
    left_join(lookup, by = c("FMS_50_ID" = "FMS_ID")) %>% 
    select(-c(FMS_50_ID, FMS_Desc)) %>% 
    rename(FMS_50 = FMS_Details) %>% 
    left_join(lookup, by = c("FMS_500_ID" = "FMS_ID")) %>% 
    select(-c(FMS_500_ID, FMS_Desc)) %>% 
    rename(FMS_500 = FMS_Details) %>% 
    mutate(across(starts_with("FMS"), ~ fct_recode(., Missing = "Not Entered")))
    # mutate(across(starts_with("FMS"), ~ fct_relevel(., "Crawling")))
  
  # Recode FAQ
  lookup <- tbls$FuncRatePar_lookup %>% 
    mutate(FuncRatePar = str_replace(FuncRatePar, "00-Unknown", "Missing")) %>% 
    select(-IsUnknown)
  
  temp <- temp %>% 
    left_join(lookup, by = c("FuncRatePar_Parent_ID" = "FuncRatePar_ID")) %>% 
    rename(FAQ = FuncRatePar) %>% 
    mutate(FAQ = fct_relevel(FAQ, "Missing", after = Inf))
  
  
  # Recode Surgical Effect
  lookup <- tbls$Effect_lookup%>% 
    mutate(Effect = str_replace(Effect, "Unknown", "Missing")) %>% 
    select(-IsUnknown)
  
  geteffect <- lookup$Effect
  names(geteffect) <- lookup$Effect_ID
  veff <- names(temp)
  veff <- str_subset(veff, "^SurgEffect.*ID$")
  for (v in veff) {
    temp[[str_replace(v,"_ID", "")]] <- factor(geteffect[as.character(temp[[v]])],
                                               levels = lookup$Effect)
  }
  
  lookup <- tbls$Outcome_lookup%>% 
    mutate(Outcome = str_replace(Outcome, "a.Unknown", "Missing")) %>% 
    mutate(Outcome = str_replace_all(Outcome, "b\\.|c\\.|d\\.|e\\.|f\\.|g\\.", "" ))
  
  getoutcome <- lookup$Outcome
  names(getoutcome) <- lookup$Outcome_ID
  
  temp$Outcome <- 
    factor(getoutcome[as.character(temp$Outcome_ID)], levels = lookup$Outcome)
  temp$OutcomePatient <-
    factor(getoutcome[as.character(temp$OutcomePatient_ID)], levels = lookup$Outcome)
  temp$SurgWorth <-
    factor(getoutcome[as.character(temp$SurgWorth_ID)], levels = lookup$Outcome)
  
  
  # Recode GMFCS
  lookup <- tbls$GMFCS_lookup%>% 
    mutate(GMFCS_Desc = str_replace(GMFCS_Desc, "0 - Unknown", "Missing")) %>% 
    select(-IsUnknown) %>% 
    mutate(GMFCS_Desc = c("Missing", "I", "II", "III", "IV", "V", "Missing"))
  
  temp <- temp %>%
    left_join(lookup) %>%
    rename(GMFCS = GMFCS_Desc) %>% 
    mutate(GMFCS = factor(GMFCS)) %>% 
    mutate(GMFCS = fct_relevel(GMFCS, "Missing", after = Inf))
  
  
  # Recode PT
  lookup <- tbls$PtFreq_lookup %>% 
    mutate(PtFreq = str_replace(PtFreq, "U-Unknown", "Missing")) %>% 
    mutate(PtFreq = str_replace_all(PtFreq, "I-|A-|C-|D-|E-|F-|G-|H-|J-|B-", "" )) %>%
    select(-IsUnknown)
  
  temp <- temp %>% 
    left_join(lookup, by = c("PtFreq_Ther_ID" = "PtFreq_ID")) %>% 
    mutate(PtFreq = fct_relevel(PtFreq, "Missing", after = Inf)) %>% 
    rename(PTfreq = PtFreq)
  
  lookup <- tbls$PtProg_lookup %>% 
    mutate(PtProg = str_replace(PtProg, "U-Unknown", "Missing")) %>% 
    mutate(PtProg = str_replace_all(PtProg, "N-|I-|A-|C-|D-|E-|F-|G-|H-|J-|B-", "" )) %>% select(-IsUnknown)
  
  temp <- temp %>% 
    left_join(lookup, by = c("PtFreq_Prog_ID" = "PtProg_id")) %>% 
    mutate(PtProg = fct_relevel(PtProg, "Missing", after = Inf)) %>% 
    rename(PTprog = PtProg)  
  
  
  # Recode Limiting Factors
  lookup <- tbls$Lim_Fac_lookup %>% 
    mutate(Lim_Factor = str_replace(Lim_Factor, "Unknown", "Missing"))
  
  getlim_fac <- lookup$Lim_Factor
  names(getlim_fac) <- lookup$Lim_Fac_ID
  vlim <- names(temp)
  vlim <- str_subset(vlim, "LIM_FAC")
  for (v in vlim) {
    temp[[v]] <- factor(getlim_fac[as.character(temp[[v]])],
                        levels = lookup$Lim_Factor)
  }
  temp <- temp %>% 
    mutate(across(all_of(vlim), ~ fct_na_value_to_level(., level = "Missing")))
  
  
  # Remove unwanted variables
  temp <- temp %>% 
    select(-(starts_with("FMS") & ends_with("ID"))) %>% 
    select(-(starts_with("FuncRate"))) %>% 
    select(-(starts_with("LimFactor"))) %>% 
    select(-(starts_with("Surg") & ends_with("_ID"))) %>% 
    select(-(matches("GMFCS_ID|Outcome_ID|OutcomePatient_ID"))) %>% 
    select(-(starts_with("Pt") & ends_with("ID"))) 
  
  FD <- FD %>% 
    left_join(temp)
  
  
  # FAQ-22 Activities
  temp <- tbls$FuncFU.tbl
  
  lookup <- tbls$FunctionalFU_Activities_lookup %>% 
    mutate(Answer_Desc = str_replace(Answer_Desc, "Unknown", "Missing")) %>% 
    mutate(Answer_Desc = str_replace(Answer_Desc, "Able on old scale", "Ignore"))
  
  getactivity <- lookup$Answer_Desc
  names(getactivity) <- lookup$Answer_ID
  vact <- names(tbls$FuncFU.tbl)
  vact <- str_subset(vact, "_ID", negate = T)
  
  for (v in vact) {
    temp[[v]] <- factor(getactivity[as.character(temp[[v]])],
                        levels = lookup$Answer_Desc)
  }
  
  temp <- temp %>% 
    mutate(across(.cols = all_of(vact), ~ fct_recode(., Missing = "Ignore")))
  
  FD <- FD %>% left_join(temp)
  
  return(FD)
}