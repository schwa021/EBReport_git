# This function is essentially a lookup table. It returns a list of features to be
# used in propensity models for a given input surgery s

get_prop_model_vars <- function(s){
  
  # Derotation Osteotomies -----
  if(s == "Femoral_Derotation_Osteotomy") prop_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", 
        "FemTor",                    # Note that FemTor is EOS femoral torsion if avail
        "BIMAL",
        "maxswi_Kne_Ang_Cor", "minswi_Kne_Ang_Cor",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn", 
        "prior_Femoral_Derotation_Osteotomy"
        )
  
  if(s == "Tibial_Derotation_Osteotomy") prop_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", 
        "FemTor",                    # Note that FemTor is EOS femoral torsion if avail
        "BIMAL",
        "maxswi_Kne_Ang_Cor", "minswi_Kne_Ang_Cor",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn", 
        "prior_Tibial_Derotation_Osteotomy")
  
  
  # Muscle Lengthening -----
  if(s == "Psoas_Release") prop_vars <- 
      c("HIP_EXT", "POP_ANG_UNI", 
        # "HIP_FLEX_STR", 
        "meansta_Pel_Ang_Sag", "maxsta_Pel_Ang_Sag", "minsta_Pel_Ang_Sag", "minsta_Hip_Ang_Sag",
        "prior_Psoas_Release"
        )
  
  if(s == "Hams_Lengthening") prop_vars <- 
      c("POP_ANG_UNI", "KNEE_EXT",
        # "meanswi_Pel_Ang_Sag",
        "ic_Hip_Ang_Sag", "ic_Kne_Ang_Sag", "ic_Pel_Ang_Sag",
        # "maxswi_Pel_Ang_Sag", "minswi_Pel_Ang_Sag",
        # "meansta_Kne_Ang_Sag",
        "minsta_Kne_Ang_Sag",
        "maxswi_Semimem_len", 
        # "maxswi_Semimem_vel",
        "prior_Hams_Lengthening")

  if(s == "Gastroc_Soleus_Lengthening") prop_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90", 
        "PLANTFLEX_SPAS", "PLANTFLEX_STR",
        "WB_FTPOS", "WB_FTPOS_SEV",
        "ic_Ank_Ang_Sag", "ic_Kne_Ang_Sag",
        # "midsta_Ank_Ang_Sag",
        "prior_Gastroc_Soleus_Lengthening")
  
  
  if(s == "Adductor_Release") prop_vars <- 
      c("HIP_ABD_0", "HIP_ABD_90", "HIP_ABD_STR",
        "ic_Hip_Ang_Cor", "maxsta_Hip_Ang_Cor", "minswi_Hip_Ang_Cor",
        "prior_Adductor_Release"
        )
  
  
  # Crouch -----
  if(s == "Patellar_Advance") prop_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        # "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag", 
        "midsta_Kne_Ang_Sag",
        "prior_Patellar_Advance", "prior_DFEO_Patellar_Advance", "prior_DFEO", "prior_Hams_Lengthening")
  
  if(s == "DFEO") prop_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        # "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag",
        "midsta_Kne_Ang_Sag",
        "prior_Patellar_Advance", "prior_DFEO_Patellar_Advance", "prior_DFEO", "prior_Hams_Lengthening")
  
  if(s == "DFEO_Patellar_Advance") prop_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        # "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag", 
        "midsta_Kne_Ang_Sag",
        "prior_Patellar_Advance", "prior_DFEO_Patellar_Advance", "prior_DFEO", "prior_Hams_Lengthening")
  
  if(s == "DFEOx") prop_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag", "midsta_Kne_Ang_Sag",
        "prior_Patellar_Advance", "prior_DFEO_Patellar_Advance", "prior_DFEO", "prior_Hams_Lengthening")
  
  
  # Spasticity -----
  if(s == "Neural_Rhizotomy") prop_vars <- 
      c("PLANTFLEX_SPAS", "HAMSTRING_SPAS", "RECT_FEM_SPAS", "ADDUCTOR_SPAS",
        "DMC",
        "GMFCS", "NETND_OXYCONS_PCT",
        "ic_Ank_Ang_Sag", "maxsta_Ank_Ang_Sag", "meanswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag",
        "prior_Neural_Rhizotomy")
  
  if(s == "Rectus_Transfer") prop_vars <- 
      c("RECT_FEM_SPAS",
        "maxswi_Kne_Ang_Sag", "fo_Kne_Ang_Sag", "ic_Kne_Ang_Sag", "romswi_Kne_Ang_Sag",
        "prior_Rectus_Transfer", "prior_Neural_Rhizotomy")
  
  
  # Foot and Ankle -----
  if(s == "Foot_and_Ankle_Bone") prop_vars <- 
      c("WB_4FTPOS", "WB_4FTPOS_SEV", "WB_4FTPOS2", "WB_4FTPOS2_SEV",
        "WB_FTPOS", "WB_FTPOS_SEV", "WB_MIDFT_POS", 
        "NWB_ARCH", "NWB_MID_MOT",
        # "NWB_FOREFT",
        # "NWB_FOREFT_SEV", 
        # "NWB_HINDFT", "NWB_HINDFT_SEV", 
        # "NWB_PF1",
        "meansta_Foo_Ang_Trn",
        "prior_Foot_and_Ankle_Bone", "prior_Foot_and_Ankle_Soft_Tissue")
  
  if(s == "Foot_and_Ankle_Soft_Tissue") prop_vars <- 
      c("WB_4FTPOS", "WB_4FTPOS_SEV", "WB_4FTPOS2", "WB_4FTPOS2_SEV",
        "WB_FTPOS",
        "WB_FTPOS_SEV",
        "WB_MIDFT_POS", "NWB_ARCH", 
        "NWB_FOREFT",
        "NWB_FOREFT_SEV",
        "NWB_HINDFT", "NWB_HINDFT_SEV", 
        "NWB_MID_MOT", 
        "NWB_PF1",
        "meansta_Foo_Ang_Trn", "meanswi_Foo_Ang_Trn",
        "prior_Foot_and_Ankle_Bone", "prior_Foot_and_Ankle_Soft_Tissue")
  
  prop_vars <- c("age", "GMFCS", "affected", "dx", "era", prop_vars)
  
  return(prop_vars)
  
}
