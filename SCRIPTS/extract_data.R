extract_data <- function(c, typestr){
  
  # Initialize empty data -----
  d <- tibble()
  
  # Get point and analog data -----
  rate <- c$Header$Video_Sampling_Rate
  ptdat <- c$VideoData
  ptlabs <- names(ptdat)
  
  # Identify all cycles bilaterally -----
  Lcyc <- find_all_strides(c)$Lcyc
  N_Lcyc <- nrow(Lcyc)
  Rcyc <- find_all_strides(c)$Rcyc
  N_Rcyc <- nrow(Rcyc)
  
  # Test for "bad" c3d file -----
  if(min(N_Lcyc, N_Rcyc) == 0 | is.null(N_Lcyc) | is.null(N_Rcyc)) return(d)   # test for no cycles
  if(nrow(c$VideoData) == 0) return(d)  # test for no 3d data
  
  # Regular expressions to find typestr labels by model -----
  # Negative lookarounds are needed to filter out weirdsies
  neg <- case_when(
    typestr == "Trunk" ~ 
      "(?!^LPel)(?!^RPel)(?!.*Global)(?!.*Abs)(?!.*Virt)(?!.*Phys)(?!.*Gol)(?!.*2FP)(?!.*FF)",
    typestr == "Moment" ~ 
      "(?!.*Ground)(?!^LPel)(?!^RPel)(?!.*Global)(?!.*Abs)(?!.*Virt)(?!.*Phys)(?!.*Gol)(?!.*2FP)(?!.*FF)",
    TRUE ~ 
      "(?!.*Rename)(?!.*Trunk)(?!.*Abs)(?!.*Virt)(?!.*Phys)(?!.*Gol)(?!.*2FP)(?!.*FF)(?!.*FootProgress)"
  )
  
  # FPA stuff -----
  Lfpastr <- "(?!.*DJC|.*PAT)(^LFootProgress)"
  Rfpastr <- "(?!.*DJC|.*PAT)(^RFootProgress)"
  
  LmatchFMCPAT <- glue("{neg}^L\\w*{typestr}\\w*DJCPAT")
  RmatchFMCPAT <- glue("{neg}^R\\w*{typestr}\\w*DJCPAT")
  LmatchFMC <- glue("{neg}^L\\w*{typestr}\\w*DJC")
  RmatchFMC <- glue("{neg}^R\\w*{typestr}\\w*DJC")
  LmatchCONV <- glue("{neg}^L\\w*{typestr}")
  RmatchCONV <- glue("{neg}^R\\w*{typestr}")
  
  if(typestr == "Length"){
    LmatchFMCPAT <- glue("{neg}(^L)\\w*(psoas_Z|semimem_Z|rect_fem_Z|add_long_Z|med_gas_Z)")
    RmatchFMCPAT <- glue("{neg}(^R)\\w*(psoas_Z|semimem_Z|rect_fem_Z|add_long_Z|med_gas_Z)")
    LmatchFMC <-  LmatchFMCPAT
    RmatchFMC <-  RmatchFMCPAT
    LmatchCONV <- LmatchFMCPAT
    RmatchCONV <- RmatchFMCPAT
  }
  
  if(typestr == "Velocity"){
    LmatchFMCPAT <- glue("{neg}(^L)\\w*(psoas_v|semimem_v|rect_fem_v|add_long_v|med_gas_v)")
    RmatchFMCPAT <- glue("{neg}(^R)\\w*(psoas_v|semimem_v|rect_fem_v|add_long_v|med_gas_v)")
    LmatchFMC <-  LmatchFMCPAT
    RmatchFMC <-  RmatchFMCPAT
    LmatchCONV <- LmatchFMCPAT
    RmatchCONV <- RmatchFMCPAT
  }
  
  # Minimum number of labels that need to be present 
  minlab <- case_when(
    typestr == "Angle" ~ 8,
    typestr == "Moment" ~ 8,
    typestr == "Power" ~ 8,
    typestr == "Reaction" ~ 5,
    typestr == "Trunk" ~ 2,
    typestr == "Length" ~ 3,
    typestr == "Velocity" ~ 3
  )
  
  # Get labels for three models (FMCPAT, FMC, CONV). Strip side to compare
  # Note: Always use CONV FootProgressAngles
  LFPA <- str_subset(ptlabs, Lfpastr) %>% str_replace_all("^L", "")
  RFPA <- str_subset(ptlabs, Lfpastr) %>% str_replace_all("^R", "")
  
  LFMCPAT <- str_subset(ptlabs, LmatchFMCPAT) %>% str_replace_all("^L", "")
  RFMCPAT <- str_subset(ptlabs, RmatchFMCPAT) %>% str_replace_all("^R", "")
  
  LFMC <- str_subset(ptlabs, LmatchFMC) %>% str_replace_all("^L", "")
  LFMC <- str_subset(LFMC, "PAT", negate = T)
  RFMC <- str_subset(ptlabs, RmatchFMC) %>% str_replace_all("^R", "")
  RFMC <- str_subset(RFMC, "PAT", negate = T)
  
  LCONV <- str_subset(ptlabs, LmatchCONV) %>% str_replace_all("^L", "")
  LCONV <- str_subset(LCONV, "DJC|PAT|Abs|Gol", negate = T)
  RCONV <- str_subset(ptlabs, RmatchCONV) %>% str_replace_all("^R", "")
  RCONV <- str_subset(RCONV, "DJC|PAT|Abs|Gol", negate = T)
  
  if(length(LCONV) == 0 | length(RCONV) == 0) return(d)
  
  
  # Fix Ankle and Pelvis Angle labels in FMC/ FMCPAT
  # If present, replace with CONV, if absent add CONV
  fixlabs <- function(lab, typestr){
    if(length(lab) > 0){
      # Replace with CONV
      lab <- str_replace_all(lab, "AnkleAnglesDJCPAT", "AnkleAngles")
      lab <- str_replace_all(lab, "PelvisAnglesDJCPAT", "PelvisAngles")
      lab <- str_replace_all(lab, "AnkleAnglesDJC", "AnkleAngles")
      lab <- str_replace_all(lab, "PelvisAnglesDJC", "PelvisAngles")
      
      # If missing then add
      if (!any(str_detect(lab, "AnkleAngles")) & typestr == "Angle") {
        lab <- c(lab, "AnkleAngles_X", "AnkleAngles_Y", "AnkleAngles_Z")
      }
      if (!any(str_detect(lab, "PelvisAngles")) & typestr == "Angle") {
        lab <- c(lab, "PelvisAngles_X", "PelvisAngles_Y", "PelvisAngles_Z")
      }
    }
    return(lab)
  }
  
  LFMC <- fixlabs(LFMC, typestr)
  RFMC <- fixlabs(RFMC, typestr)
  LFMCPAT <- fixlabs(LFMCPAT, typestr)
  RFMCPAT <- fixlabs(RFMCPAT, typestr)
  
  
  # Pick Model and vnames ------------------------------------------------------
  
  v <- NULL
  modname <- NULL
  if(all(LFMCPAT %in% RFMCPAT) & all(RFMCPAT %in% LFMCPAT) & length(LFMCPAT) > minlab){
    if(typestr == "Angle"){
      v <- c(LFMCPAT, LFPA)
    } else{
      v <- LFMCPAT
    }
    modname <- "FMCPAT"
  } else   if(all(LFMC %in% RFMC) & all(RFMC %in% LFMC) & length(LFMC) > minlab){
    if(typestr == "Angle"){
      v <- c(LFMC, LFPA)
    } else{
      v <- LFMC
    }
    modname <- "FMC"
  } else if(all(LCONV %in% RCONV) & all(RCONV %in% LCONV) & length(LCONV) > minlab){
    if(typestr == "Angle"){
      v <- c(LCONV, LFPA)
    } else{
      v <- LCONV
    }
    modname <- "CONV"
  }
  
  
  #Extract Data ----------------------------------------------------------------
  if(!is.null(v) & length(v) > minlab){
    d <-
      map(
        .x = 1:min(N_Lcyc, N_Rcyc), 
        ~ get_cycle_data(c, vnames = v, Lcyc, Rcyc, typestr, .x)
      ) %>%
      list_rbind() %>%
      mutate(across(.cols = matches("^FootProg\\w*_X$"), ~ -(.x + 90)), model = modname)
  }
  
  
  # Return data in list --------------------------------------------------------
  return(d)
  
}

