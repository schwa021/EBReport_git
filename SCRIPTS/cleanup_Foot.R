## Clean up FOOT

# The main things happening here are (1) splitting position and severity, and (2)
# lumping (or dropping) some levels of positions. I also add a severity ("NONE")
# associated with a "TYP" position

cleanup_Foot <- function(FD) {
  # FIR_MTP_DF
  FD <- 
    FD %>%
    mutate(FIR_MTP_DF = factor(FIR_MTP_DF, levels = c("RES", "TYP", "HYP", "Missing")))
  
  
  # NWB_FOREFT
  FD <- 
    FD %>%
    mutate(NWB_FOREFT = toupper(NWB_FOREFT)) %>%
    mutate(
      NWB_PF1 = case_when(
        NWB_FOREFT == "VAL/PF1/MOB" ~ "MIL",
        NWB_FOREFT == "VAL/PF1/RIG" ~ "SEV",
        NWB_FOREFT == "VAL/PF1/SEM" ~ "MOD",
        NWB_FOREFT == "NEU/PF1/VAR" ~ "Missing",
        NWB_FOREFT == "-99" ~ "Missing",
        TRUE ~ "TYP"
      )
    ) %>%
    mutate(NWB_PF1 = factor(NWB_PF1, levels = c("TYP", "MIL", "MOD", "SEV", "Missing"))) %>%
    mutate(
      NWB_FOREFT = case_when(
        NWB_FOREFT == "NEU/PF1/VAR" ~ "NEU",
        NWB_FOREFT == "VAL/PF1/MOB" ~ "VAL/MIL",
        NWB_FOREFT == "VAL/PF1/RIG" ~ "VAL/SEV",
        NWB_FOREFT == "VAL/PF1/SEM" ~ "VAL/MOD",
        TRUE ~ NWB_FOREFT
      )
    ) %>%
    mutate(NWB_FOREFT = str_remove(NWB_FOREFT, "/TOT"))
  
  
  # temp <- FD
  v <- names(FD)
  vnwb <- str_subset(v, "^NWB")
  vwb <- str_subset(v, "^WB")
  
  temp <- 
    FD %>% 
    select(all_of(c(vnwb, vwb)))
  
  temp <- 
    temp %>%
    mutate(across(everything(), toupper)) %>%
    mutate(across(everything(), ~ ifelse(. == -99, NA, .)))
  
  
  # Function to recode foot data
  footrecode <- function(x, pos, poslev, sevlev, tdlev) {
    # Get name of severity
    sev <- paste0(pos, "_SEV")
    
    # Define levels for position
    newlev <- str_split(poslev, "\\|")[[1]]
    
    # Extract severity and position
    x[[sev]] <- factor(str_extract(x[[pos]], sevlev))
    x[[sev]] <- fct_expand(x[[sev]], "NONE")
    
    x[[pos]] <- factor(str_extract(x[[pos]], poslev), levels = newlev)
    
    # Make severity of TD position equal to NONE, and make this first level
    x[[sev]][x[[pos]] == tdlev] <- "NONE"
    
    # Make NA explicit
    x[[sev]] <- fct_na_value_to_level(x[[sev]], level = "Missing")
    x[[pos]] <- fct_na_value_to_level(x[[pos]], level = "Missing")
    
    return(x)
  }
  
  # Recode NWB and WB measures
  temp <- footrecode(temp, "NWB_FOREFT", "VAL|NEU|VAR", "MIL|MOD|SEV", "NEU")
  temp <- footrecode(temp, "NWB_FOREFT2", "ABD|NEU|ADD", "MIL|MOD|SEV", "NEU")
  temp <- footrecode(temp, "NWB_HINDFT", "VAL|VER|VAR", "MIL|MOD|SEV", "VER")
  temp <- footrecode(temp, "WB_4FTPOS", "VAL|TYP|VAR", "MIL|MOD|SEV", "TYP")
  temp <- footrecode(temp, "WB_4FTPOS2", "ABD|TYP|ADD", "MIL|MOD|SEV", "TYP")
  temp <- footrecode(
    temp, "WB_FTPOS", "CAL|CVL|CVR|EQU|EVL|EVR|TYP|VAL|VAR",
    "MIL|MOD|SEV", "TYP"
  )
  
  templist <- c("WB_MIDFT_POS", "NWB_ARCH", "NWB_HIND_EVER", "NWB_HIND_INVER", 
                "NWB_MID_MOT", "NWB_SUBTAL_NEUT")
  
  temp <- 
    temp %>%
    mutate(WB_MIDFT_POS = factor(WB_MIDFT_POS,
                                 levels = c("PLA", "TYP", "CAV")
    )) %>%
    mutate(NWB_ARCH = factor(NWB_ARCH,
                             levels = c("LO", "TYP", "HI")
    )) %>%
    mutate(NWB_HIND_EVER = factor(NWB_HIND_EVER,
                                  levels = c("RES", "TYP", "HYP")
    )) %>%
    mutate(NWB_HIND_INVER = factor(NWB_HIND_INVER,
                                   levels = c("RES", "TYP", "HYP")
    )) %>%
    mutate(NWB_MID_MOT = factor(NWB_MID_MOT,
                                levels = c("RES", "TYP", "HYP")
    )) %>%
    mutate(NWB_SUBTAL_NEUT = factor(NWB_SUBTAL_NEUT,
                                    levels = c("N", "Y", "U")
    )) %>%
    mutate(NWB_SUBTAL_NEUT = fct_recode(NWB_SUBTAL_NEUT, Missing = "U")) %>%
    mutate(across(
      all_of(templist),
      ~ fct_na_value_to_level(., level = "Missing")
    ))
  
  
  FD[names(temp)] <- temp
  
  v <- sort(str_subset(names(FD), "WB|NWB"))
  
  FD <- FD %>%
    relocate(all_of(v), .after = THOMAS)
  
  return(FD)
}