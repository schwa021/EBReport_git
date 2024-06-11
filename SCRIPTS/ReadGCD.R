# Function to read GCD and format data

ReadGCD <- function(TrialInfo, kk){
  
  f = TrialInfo$Filename[kk]
  Patient_ID = TrialInfo$Patient_ID[kk]
  Exam_ID = TrialInfo$Exam_ID[kk]
  Sex = TrialInfo$Sex[kk]
  Age = TrialInfo$Age[kk]
  Year = year(TrialInfo$Event_Date[kk])
  Trial_Num = TrialInfo$Trial_Num[kk]
  TrialType_ID = TrialInfo$TrialType_ID[kk]
  mass <- TrialInfo$WEIGHT[kk]
  leglenL <- TrialInfo$LEG_LENGTH_L[kk]
  leglenR <- TrialInfo$LEG_LENGTH_R[kk]
  
  # Read in file ---------------------------------------------------------------
  g <- read_lines(f, n_max = -1)
  
  
  # Data Name Roots ------------------------------------------------------------
  # These are used to build regex's for finding the data. Also, they are ordered
  # to make factor leveling easier
  vAng <- c("PelvicObliquity", "PelvicTilt", "PelvicRotation", 
            "HipAbAdduct", "HipFlexExt", "HipRotation",
            "KneeValgVar", "KneeFlexExt", "KneeRotation",
            "DorsiPlanFlex", "FootRotation",
            "FootProgression")
  
  vMom <- c("HipAbAdduct", "HipFlexExt", "HipRotation",
            "KneeValgVar", "KneeFlexExt", "KneeRotation",
            "FootAbAdduct", "DorsiPlanFlex", "FootRotation")
  
  vPwr <- c("Hip", "Knee", "Ankle")
  
  
  # Extract Angles -------------------------------------------------------------
  vAngL <- glue("(?!.*Moment)(?!.*Power)Left{vAng}")
  vAngR <- glue("(?!.*Moment)(?!.*Power)Right{vAng}")
  iL <- vAngL %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  iR <- vAngR %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  AngL <- iL %>% map(\(i) g[(i+1):(i+51)]) %>% unlist() %>% as.numeric()
  AngR <- iR %>% map(\(i) g[(i+1):(i+51)]) %>% unlist() %>% as.numeric()
  
  # Weird all zeros?
  if(all(AngL == 0) | all(AngR ==0)){
    return(list(
      "ang" = tibble(),
      "mom" = tibble(),
      "pwr" = tibble(),
      "rxn" = tibble()
    ))
  }
  
  # Insert missing angles to match c3d extraction
  AngL <- c(AngL[1:(9 * 51)],
            rep(NA, 51), AngL[((9 * 51) + 1):(11 * 51)],
            rep(NA, (2 * 51)), AngL[((11 * 51) + 1):(12 * 51)])
  AngR <- c(AngR[1:(9 * 51)],
            rep(NA, 51), AngR[((9 * 51) + 1):(11 * 51)],
            rep(NA, (2 * 51)), AngR[((11 * 51) + 1):(12 * 51)])
  
  # Extract Moments ------------------------------------------------------------
  vMomL <- glue("Left{vMom}\\w*Moment$")
  vMomR <- glue("Right{vMom}\\w*Moment$")
  iL <- vMomL %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  iR <- vMomR %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  MomL <- iL %>% map(\(i) g[(i+1):(i+51)]) %>% unlist()
  if(is.null(MomL)) MomL <- rep(NA, 51 * 9)
  MomR <- iR %>% map(\(i) g[(i+1):(i+51)]) %>% unlist()  
  if(is.null(MomR)) MomR <- rep(NA, 51 * 9)
  
  # Extract Power --------------------------------------------------------------
  vPwrL <- glue("(?!.*Flex)(?!.*Ab)(?!.*Rot)(?!.*Val)Left{vPwr}\\w*Power$")
  vPwrR <- glue("(?!.*Flex)(?!.*Ab)(?!.*Rot)(?!.*Val)Right{vPwr}\\w*Power$")
  iL <- vPwrL %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  iR <- vPwrR %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  PwrL <- iL %>% map(\(i) g[(i+1):(i+51)]) %>% unlist()
  if(is.null(PwrL)) PwrL <- rep(NA, 51 * 3)
  PwrR <- iR %>% map(\(i) g[(i+1):(i+51)]) %>% unlist()    
  if(is.null(PwrR)) PwrR <- rep(NA, 51 * 3)
  
  # Extract Reaction -----------------------------------------------------------
  vRxnL <- glue("Left\\w*Reaction")
  vRxnR <- glue("Right\\w*Reaction")
  iL <- vRxnL %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  iR <- vRxnR %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  RxnL <- iL %>% map(\(i) g[(i+1):(i+51)]) %>% unlist()
  RxnR <- iR %>% map(\(i) g[(i+1):(i+51)]) %>% unlist()
  RxnL <- 1:51 %>% map(\(x) str_split_fixed(RxnL[x], " ", 6)) %>% unlist()
  RxnR <- 1:51 %>% map(\(x) str_split_fixed(RxnR[x], " ", 6)) %>% unlist()
  
  # Reshape since the Rxn is stored 6-wide
  RxnL <- matrix(data = as.numeric(RxnL), nrow = 51, ncol = 6, byrow = T)
  RxnR <- matrix(data = as.numeric(RxnR), nrow = 51, ncol = 6, byrow = T)
  dim(RxnL) <- c(306, 1)
  dim(RxnR) <- c(306, 1)
  if(is.null(RxnL)) RxnL <- rep(NA, 51 * 6)
  if(is.null(RxnR)) RxnR <- rep(NA, 51 * 3)
  
  # Extract events -------------------------------------------------------------
  vEvt <- c("OppositeFootOff", "OppositeFootContact", "FootOff")
  vEvtL <- glue("Left{vEvt}")
  vEvtR <- glue("Right{vEvt}")
  iL <- vEvtL %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  iR <- vEvtR %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  EvtL <- iL %>% map(\(i) g[(i+1)]) %>% unlist() %>% as.numeric()
  EvtR <- iR %>% map(\(i) g[(i+1)]) %>% unlist() %>% as.numeric()
  EvtL <- 2 * round(EvtL / 2)
  EvtR <- 2 * round(EvtR / 2)
  
  # Extract step parameters ----------------------------------------------------
  vPrm <- c("StepLength", "Speed", "Stridelength")
  vPrmL <- glue("(?i)Left{vPrm}")
  vPrmR <- glue("(?i)Right{vPrm}")
  iL <- vPrmL %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  iR <- vPrmR %>% map(\(x) which(str_detect(g, x))) %>% unlist()
  PrmL <- iL %>% map(\(i) g[(i+1)]) %>% unlist() %>% as.numeric()
  PrmR <- iR %>% map(\(i) g[(i+1)]) %>% unlist() %>% as.numeric()
  PrmL <- PrmL / 1000
  PrmR <- PrmR / 1000
  
  
  
  # Build Ang, Mom, Pwr, Rxn, Trk Tibbles --------------------------------------
  Angnames <- c(
    rep("Pel.Ang.Cor", 51), rep("Pel.Ang.Sag", 51), rep("Pel.Ang.Trn", 51),
    rep("Hip.Ang.Cor", 51), rep("Hip.Ang.Sag", 51), rep("Hip.Ang.Trn", 51),
    rep("Kne.Ang.Cor", 51), rep("Kne.Ang.Sag", 51), rep("Kne.Ang.Trn", 51),
    rep("Ank.Ang.Cor", 51), rep("Ank.Ang.Sag", 51), rep("Ank.Ang.Trn", 51),
    rep("Foo.Ang.Cor", 51), rep("Foo.Ang.Sag", 51), rep("Foo.Ang.Trn", 51)
  ) %>% fct_inorder()
  
  Momnames <- c(
    rep("Hip.Mom.Cor", 51), rep("Hip.Mom.Sag", 51), rep("Hip.Mom.Trn", 51),
    rep("Kne.Mom.Cor", 51), rep("Kne.Mom.Sag", 51), rep("Kne.Mom.Trn", 51),
    rep("Ank.Mom.Cor", 51), rep("Ank.Mom.Sag", 51), rep("Ank.Mom.Trn", 51)
  ) %>% fct_inorder()
  
  Pwrnames <- c(
    rep("Hip.Pwr", 51), rep("Kne.Pwr", 51), rep("Ank.Pwr", 51)
  ) %>% fct_inorder()
  
  Rxnnames <- c(
    rep("GRF.AntPst", 51), rep("GRF.MedLat", 51), rep("GRF.UpDwn", 51),
    rep("GRM.AntPst", 51), rep("GRM.MedLat", 51), rep("GRM.UpDwn", 51)
  ) %>% fct_inorder()
  
  
  # Check that proper amount of data is present and build tibbles --------------
  
  if(length(c(AngL, AngR)) != 51 * 15 * 2){
    AngL <- rep(NA, 51 * 15)
    AngR <- rep(NA, 51 * 15)
  }
  if(length(c(MomL, MomR)) != 51 * 9 * 2){
    MomL <- rep(NA, 51 * 9)
    MomR <- rep(NA, 51 * 9)
  }
  if(length(c(PwrL, PwrR)) != 51 * 3 * 2){
    PwrL <- rep(NA, 51 * 3)
    PwrR <- rep(NA, 51 * 3)
  }
  if(length(c(RxnL, RxnR)) != 51 * 6 * 2){
    RxnL <- rep(NA, 51 * 6)
    RxnR <- rep(NA, 51 * 6)
  }
  
  # Build orthdev and assdev variables
  angrows <- 51 * 15 * 2
  orthdev <- 
    c(rep(TrialInfo$OrthDev_L[kk], angrows/2), rep(TrialInfo$OrthDev_R[kk], angrows/2))
  assdev <- 
    c(rep(TrialInfo$AssistDev_L[kk], angrows/2), rep(TrialInfo$AssistDev_R[kk], angrows/2))
  
  ang <- tibble(
    .rows = 51 * 15 * 2,
    "Patient_ID" = as.integer(Patient_ID),
    "Exam_ID" = as.integer(Exam_ID),
    "Sex" = Sex,
    "Age" = Age,
    "Year" = Year,
    "Trial_Num" = as.integer(Trial_Num),
    "cycle" = as.integer(1),
    "side" = c(rep("L", 51 * 15), rep("R", 51 * 15)),
    "t" = rep(seq(0, 100, 2), 15 * 2),
    "name" = rep(Angnames, 2),
    "value" = as.numeric(c(AngL, AngR)),
    "OFO" = c(rep(EvtL[1], 51 * 15), rep(EvtR[1], 51 * 15)),
    "OFC" = c(rep(EvtL[2], 51 * 15), rep(EvtR[2], 51 * 15)),
    "FO" = c(rep(EvtL[3], 51 * 15), rep(EvtR[3], 51 * 15)),
    "speed" = c(rep(PrmL[2], 51 * 15), rep(PrmR[2], 51 * 15)),
    "steplen" = c(rep(PrmL[1], 51 * 15), rep(PrmR[1], 51 * 15)),
    "stridelen" = c(rep(PrmL[3], 51 * 15), rep(PrmR[3], 51 * 15)),
    "mass" = rep(mass, 51 * 15 * 2),
    "leglen" = c(rep(leglenL, 51 * 15), rep(leglenR, 51 * 15)),
    "TrialType_ID" = as.integer(TrialType_ID),
    "model" = "CONV",
    "var" = "Angle",
    "orthdev" = orthdev,
    "assdev" = assdev
  )
  ang$oppsteplen <- ang$stridelen - ang$steplen

  
  momrows <- 51 * 9 * 2
  orthdev <- 
    c(rep(TrialInfo$OrthDev_L[kk], momrows/2), rep(TrialInfo$OrthDev_R[kk], momrows/2))
  assdev <- 
    c(rep(TrialInfo$AssistDev_L[kk], momrows/2), rep(TrialInfo$AssistDev_R[kk], momrows/2))
  
  mom <- tibble(
    .rows = 51 * 9 * 2,
    "Patient_ID" = as.integer(Patient_ID),
    "Exam_ID" = as.integer(Exam_ID),
    "Sex" = Sex,
    "Age" = Age,
    "Year" = Year,
    "Trial_Num" = as.integer(Trial_Num),
    "cycle" = as.integer(1),
    "side" = c(rep("L", 51 * 9), rep("R", 51 * 9)),
    "t" = rep(seq(0, 100, 2), 9 * 2),
    "name" = rep(Momnames, 2),
    "value" = as.numeric(c(MomL, MomR)),
    "OFO" = c(rep(EvtL[1], 51 * 9), rep(EvtR[1], 51 * 9)),
    "OFC" = c(rep(EvtL[2], 51 * 9), rep(EvtR[2], 51 * 9)),
    "FO" = c(rep(EvtL[3], 51 * 9), rep(EvtR[3], 51 * 9)),
    "speed" = c(rep(PrmL[2], 51 * 9), rep(PrmR[2], 51 * 9)),
    "steplen" = c(rep(PrmL[1], 51 * 9), rep(PrmR[1], 51 * 9)),
    "stridelen" = c(rep(PrmL[3], 51 * 9), rep(PrmR[3], 51 * 9)),
    "mass" = rep(mass, 51 * 9 * 2),
    "leglen" = c(rep(leglenL, 51 * 9), rep(leglenR, 51 * 9)),
    "TrialType_ID" = as.integer(TrialType_ID),
    "model" = "CONV",
    "var" = "Moment",
    "orthdev" = orthdev,
    "assdev" = assdev
  )
  mom$oppsteplen <- mom$stridelen - mom$steplen
  
  pwrrows <- 51 * 3 * 2
  orthdev <- 
    c(rep(TrialInfo$OrthDev_L[kk], pwrrows/2), rep(TrialInfo$OrthDev_R[kk], pwrrows/2))
  assdev <- 
    c(rep(TrialInfo$AssistDev_L[kk], pwrrows/2), rep(TrialInfo$AssistDev_R[kk], pwrrows/2))
  
  
  pwr <- tibble(
    .rows = 51 * 3 * 2,
    "Patient_ID" = as.integer(Patient_ID),
    "Exam_ID" = as.integer(Exam_ID),
    "Sex" = Sex,
    "Age" = Age,
    "Year" = Year,
    "Trial_Num" = as.integer(Trial_Num),
    "cycle" = as.integer(1),
    "side" = c(rep("L", 51 * 3), rep("R", 51 * 3)),
    "t" = rep(seq(0, 100, 2), 3 * 2),
    "name" = rep(Pwrnames, 2),
    "value" = as.numeric(c(PwrL, PwrR)),
    "OFO" = c(rep(EvtL[1], 51 * 3), rep(EvtR[1], 51 * 3)),
    "OFC" = c(rep(EvtL[2], 51 * 3), rep(EvtR[2], 51 * 3)),
    "FO" = c(rep(EvtL[3], 51 * 3), rep(EvtR[3], 51 * 3)),
    "speed" = c(rep(PrmL[2], 51 * 3), rep(PrmR[2], 51 * 3)),
    "steplen" = c(rep(PrmL[1], 51 * 3), rep(PrmR[1], 51 * 3)),
    "stridelen" = c(rep(PrmL[3], 51 * 3), rep(PrmR[3], 51 * 3)),
    "mass" = rep(mass, 51 * 3 * 2),
    "leglen" = c(rep(leglenL, 51 * 3), rep(leglenR, 51 * 3)),
    "TrialType_ID" = as.integer(TrialType_ID),
    "model" = "CONV",
    "var" = "Power",
    "orthdev" = orthdev,
    "assdev" = assdev
  )
  pwr$oppsteplen <- pwr$stridelen - pwr$steplen
  
  
  rxnrows <- 51 * 6 * 2
  orthdev <- 
    c(rep(TrialInfo$OrthDev_L[kk], rxnrows/2), rep(TrialInfo$OrthDev_R[kk], rxnrows/2))
  assdev <- 
    c(rep(TrialInfo$AssistDev_L[kk], rxnrows/2), rep(TrialInfo$AssistDev_R[kk], rxnrows/2))
  
  
  rxn <- tibble(
    .rows = 51 * 6 * 2,
    "Patient_ID" = as.integer(Patient_ID),
    "Exam_ID" = as.integer(Exam_ID),
    "Sex" = Sex,
    "Age" = Age,
    "Year" = Year,
    "Trial_Num" = as.integer(Trial_Num),
    "cycle" = as.integer(1),
    "side" = c(rep("L", 51 * 6), rep("R", 51 * 6)),
    "t" = rep(seq(0, 100, 2), 6 * 2),
    "name" = rep(Rxnnames, 2),
    "value" = as.numeric(c(RxnL, RxnR)),
    "OFO" = c(rep(EvtL[1], 51 * 6), rep(EvtR[1], 51 * 6)),
    "OFC" = c(rep(EvtL[2], 51 * 6), rep(EvtR[2], 51 * 6)),
    "FO" = c(rep(EvtL[3], 51 * 6), rep(EvtR[3], 51 * 6)),
    "speed" = c(rep(PrmL[2], 51 * 6), rep(PrmR[2], 51 * 6)),
    "steplen" = c(rep(PrmL[1], 51 * 6), rep(PrmR[1], 51 * 6)),
    "stridelen" = c(rep(PrmL[3], 51 * 6), rep(PrmR[3], 51 * 6)),
    "mass" = rep(mass, 51 * 6 * 2),
    "leglen" = c(rep(leglenL, 51 * 6), rep(leglenR, 51 * 6)),
    "TrialType_ID" = as.integer(TrialType_ID),
    "model" = "CONV",
    "var" = "Reaction",
    "orthdev" = orthdev,
    "assdev" = assdev
  )
  rxn$oppsteplen <- rxn$stridelen - rxn$steplen
  
  return(list(
    "ang" = ang,
    "mom" = mom,
    "pwr" = pwr,
    "rxn" = rxn
  ))
  
}
