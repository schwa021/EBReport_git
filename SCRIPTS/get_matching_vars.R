# This function selects variables to be used in outcome prediction (match or bart)

get_matching_vars <- function(s){
  
  # Derotation Osteotomies -----
  if(s == "Femoral_Derotation_Osteotomy") match_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(s == "Tibial_Derotation_Osteotomy") match_vars <- 
      c("BIMAL", 
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  
  # Muscle Lengthening -----
  if(s == "Psoas_Release") match_vars <- 
      c("HIP_EXT", "POP_ANG_UNI", 
        "meansta_Pel_Ang_Sag", "maxsta_Pel_Ang_Sag", "minsta_Pel_Ang_Sag", "minsta_Hip_Ang_Sag"
      )
  
  if(s == "Hams_Lengthening") match_vars <- 
      c("POP_ANG_UNI", 
        "ic_Kne_Ang_Sag", "ic_Pel_Ang_Sag",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(s == "Gastroc_Soleus_Lengthening") match_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90",
        "ic_Ank_Ang_Sag", "maxsta_Ank_Ang_Sag", "minswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  
  if(s == "Adductor_Release") match_vars <- 
      c("HIP_ABD_0", 
        "ic_Hip_Ang_Cor", "maxsta_Hip_Ang_Cor", "minswi_Hip_Ang_Cor")
  
  
  # Crouch -----
  if(s == "Patellar_Advance") match_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(s == "DFEO") match_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  
  # Spasticity -----
  if(s == "Neural_Rhizotomy") match_vars <- 
      c("age", "GMFCS",
        "PLANTFLEX_SPAS", "HAMSTRING_SPAS", "RECT_FEM_SPAS", "ADDUCTOR_SPAS",
        "ic_Ank_Ang_Sag", "maxsta_Ank_Ang_Sag", "meanswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "maxswi_Kne_Ang_Sag")
  
  if(s == "Rectus_Transfer") match_vars <- 
      c("RECT_FEM_SPAS",
        "maxswi_Kne_Ang_Sag", "ic_Kne_Ang_Sag")
  
  
  # Foot and Ankle -----
  if(s == "Foot_and_Ankle_Bone") match_vars <- 
      c("WB_4FTPOS", "WB_4FTPOS_SEV", "WB_4FTPOS2", "WB_4FTPOS2_SEV",
        "WB_FTPOS", "WB_FTPOS_SEV", "WB_MIDFT_POS", 
        "meansta_Foo_Ang_Trn")
  
  if(s == "Foot_and_Ankle_Soft_Tissue") match_vars <- 
      c("NWB_ARCH", "NWB_FOREFT",
        "NWB_FOREFT_SEV", "NWB_HINDFT", "NWB_HINDFT_SEV", "NWB_MID_MOT", "NWB_PF1",
        "meansta_Foo_Ang_Trn", "meanswi_Foo_Ang_Trn")
  
  match_vars <- c(match_vars)
  
  return(match_vars)
  
}
