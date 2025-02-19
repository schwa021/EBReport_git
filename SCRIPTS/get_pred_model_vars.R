# This function selects variables to be used in outcome prediction (match or bart)

get_pred_model_vars <- function(v){
  
  # Anteversion and Related Measures -----
  if(v == "ANTEVERSION") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(v == "FemTor") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "FemTor", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(v == "HIP_EXT_ROT") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(v == "HIP_INT_ROT") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(v == "meansta_Hip_Ang_Trn") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(v == "meansta_Foo_Ang_Trn") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  
  # Tibial Torsion and Related Measures -----  
  if(v == "BIMAL") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  if(v == "meansta_Kne_Ang_Trn") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn")
  
  
  # Hip Flexion Contracture -----
  if(v == "HIP_EXT") pred_vars <- 
      c("HIP_EXT", "POP_ANG_UNI", 
        "meansta_Pel_Ang_Sag", "maxsta_Pel_Ang_Sag", "minsta_Pel_Ang_Sag", "minsta_Hip_Ang_Sag")
  
  if(v == "minsta_Hip_Ang_Sag") pred_vars <- 
      c("HIP_EXT", "POP_ANG_UNI", 
        "meansta_Pel_Ang_Sag", "maxsta_Pel_Ang_Sag", "minsta_Pel_Ang_Sag", "minsta_Hip_Ang_Sag")
  
  if(v == "meansta_Pel_Ang_Sag") pred_vars <- 
      c("HIP_EXT", "POP_ANG_UNI", "HIP_EXT_STR",
        "meansta_Pel_Ang_Sag", "maxsta_Pel_Ang_Sag", "minsta_Pel_Ang_Sag", 
        "meansta_Hip_Ang_Sag", "maxsta_Hip_Ang_Sag", "minsta_Hip_Ang_Sag") 
  
  # Hamstrings Contracture -----  
  if(v == "POP_ANG_UNI") pred_vars <- 
      c("POP_ANG_UNI", 
        "ic_Kne_Ang_Sag", "ic_Pel_Ang_Sag",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(v == "ic_Kne_Ang_Sag") pred_vars <- 
      c("POP_ANG_UNI", 
        "ic_Kne_Ang_Sag", "ic_Pel_Ang_Sag",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")  
  
  
  # Hip Abductor Contracture -----
  if(v == "HIP_ABD_0") pred_vars <- 
      c("HIP_ABD_0", 
        "ic_Hip_Ang_Cor", "maxsta_Hip_Ang_Cor", "minswi_Hip_Ang_Cor")
  
  if(v == "minswi_Hip_Ang_Cor") pred_vars <- 
      c("HIP_ABD_0", 
        "ic_Hip_Ang_Cor", "maxsta_Hip_Ang_Cor", "minswi_Hip_Ang_Cor")
  
  if(v == "maxsta_Hip_Ang_Cor") pred_vars <- 
      c("HIP_ABD_0", 
        "ic_Hip_Ang_Cor", "maxsta_Hip_Ang_Cor", "minswi_Hip_Ang_Cor") 
  
  if(v == "meansta_Hip_Ang_Cor") pred_vars <- 
      c("HIP_ABD_0", 
        "ic_Pel_Ang_Cor", "maxsta_Pel_Ang_Cor", "minswi_Pel_Ang_Cor",
        "ic_Hip_Ang_Cor", "maxsta_Hip_Ang_Cor", "minswi_Hip_Ang_Cor", "meansta_Hip_Ang_Cor")  
  
  if(v == "maxsta_Pel_Ang_Cor") pred_vars <- 
      c("HIP_ABD_0", 
        "ic_Pel_Ang_Cor", "maxsta_Pel_Ang_Cor", "minswi_Pel_Ang_Cor")

  
  # Calf Muscle Contracture -----
  if(v == "ANK_DORS_0") pred_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90",
        "ic_Ank_Ang_Sag", "maxsta_Ank_Ang_Sag", "minswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(v == "maxsta_Ank_Ang_Sag") pred_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90",
        "ic_Ank_Ang_Sag", "maxsta_Ank_Ang_Sag", "minswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(v == "midsta_Ank_Ang_Sag") pred_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90",
        "ic_Ank_Ang_Sag", "midsta_Ank_Ang_Sag", "minswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(v == "ic_Ank_Ang_Sag") pred_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90",
        "ic_Ank_Ang_Sag", "midsta_Ank_Ang_Sag", "minswi_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(v == "minsta_Ank_Ang_Sag") pred_vars <- 
      c("ANK_DORS_0", "ANK_DORS_90",
        "ic_Ank_Ang_Sag", "midsta_Ank_Ang_Sag", "minswi_Ank_Ang_Sag", "minsta_Ank_Ang_Sag",
        "ic_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  
  # Knee Flexion Contracture/CROUCH -----
  if(v == "KNEE_EXT") pred_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  if(v == "EXTEN_LAG") pred_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")  
  
  if(v == "minsta_Kne_Ang_Sag") pred_vars <- 
      c("KNEE_EXT", "PATELLA_ALTA", "EXTEN_LAG",
        "meansta_Kne_Ang_Sag", "minsta_Kne_Ang_Sag")
  
  
  # Spasticity -----
  if(v == "avgspa") pred_vars <- 
      c("avgspa", "PLANTFLEX_SPAS", "HAMSTRING_SPAS", "RECT_FEM_SPAS", "ADDUCTOR_SPAS")
  
  if(v == "KNEE_FLEX") pred_vars <- 
      c("KNEE_FLEX", "RECT_FEM_SPAS", 
        "maxswi_Kne_Ang_Sag", "ic_Kne_Ang_Sag", "meansta_Kne_Ang_Sag")
  
  if(v == "maxswi_Kne_Ang_Sag") pred_vars <- 
      c("avgspa", "RECT_FEM_SPAS",
        "maxswi_Kne_Ang_Sag", "ic_Kne_Ang_Sag", "meansta_Kne_Ang_Sag")
  
  if(v == "meansta_Ank_Ang_Sag") pred_vars <- 
      c("meansta_Ank_Ang_Sag", "avgspa", "PLANTFLEX_SPAS", "HAMSTRING_SPAS",
        "ic_Ank_Ang_Sag", "meanswi_Ank_Ang_Sag",
        "maxswi_Kne_Ang_Sag", "ic_Kne_Ang_Sag", "meansta_Kne_Ang_Sag")
  
  
  # Foot and Ankle -----
  if(v == "avgwb") pred_vars <- 
      c("avgwb", "avgnwb", "WB_4FTPOS", "WB_4FTPOS_SEV", "WB_4FTPOS2", "WB_4FTPOS2_SEV",
        "WB_FTPOS", "WB_FTPOS_SEV", "WB_MIDFT_POS", 
        "meansta_Foo_Ang_Trn", "meanswi_Foo_Ang_Trn")
  
  if(v == "avgnwb") pred_vars <- 
      c("avgwb", "avgnwb", "NWB_ARCH", "NWB_FOREFT",
        "NWB_FOREFT_SEV", "NWB_HINDFT", "NWB_HINDFT_SEV", "NWB_MID_MOT", "NWB_PF1",
        "meansta_Foo_Ang_Trn", "meanswi_Foo_Ang_Trn")
  
  if(v == "meanswi_Foo_Ang_Trn") pred_vars <- 
      c("HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "BIMAL",
        "EOS_FemTor", "EOS_Bimal",
        "avgwb", "avgnwb",
        "meansta_Pel_Ang_Trn", "meansta_Hip_Ang_Trn", "meansta_Kne_Ang_Trn", "meansta_Foo_Ang_Trn",
        "meanswi_Pel_Ang_Trn", "meanswi_Hip_Ang_Trn", "meanswi_Kne_Ang_Trn", "meanswi_Foo_Ang_Trn")
  
  # Overall -----
  if(v == "GDI") pred_vars <- 
      c("GDI",
        "avgspa", "avgwb", "avgnwb",
        "HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "EOS_FemTor",
        "BIMAL", "EOS_Bimal",
        "HIP_EXT", "POP_ANG_UNI",
        "KNEE_EXT", "EXTEN_LAG", "PATELLA_ALTA",
        "ANK_DORS_0", "ANK_DORS_90",
        
        "ic_Pel_Ang_Sag", "ofo_Pel_Ang_Sag", "ofc_Pel_Ang_Sag", "fo_Pel_Ang_Sag", "midswi_Pel_Ang_Sag",
        "ic_Hip_Ang_Sag", "ofo_Hip_Ang_Sag", "ofc_Hip_Ang_Sag", "fo_Hip_Ang_Sag", "midswi_Hip_Ang_Sag",
        "ic_Kne_Ang_Sag", "ofo_Kne_Ang_Sag", "ofc_Kne_Ang_Sag", "fo_Kne_Ang_Sag", "midswi_Kne_Ang_Sag",
        "ic_Ank_Ang_Sag", "ofo_Ank_Ang_Sag", "ofc_Ank_Ang_Sag", "fo_Ank_Ang_Sag", "midswi_Ank_Ang_Sag",
        
        "ic_Pel_Ang_Cor", "ofo_Pel_Ang_Cor", "ofc_Pel_Ang_Cor", "fo_Pel_Ang_Cor", "midswi_Pel_Ang_Cor",
        "ic_Hip_Ang_Cor", "ofo_Hip_Ang_Cor", "ofc_Hip_Ang_Cor", "fo_Hip_Ang_Cor", "midswi_Hip_Ang_Cor",
        
        "ic_Pel_Ang_Trn", "ofo_Pel_Ang_Trn", "ofc_Pel_Ang_Trn", "fo_Pel_Ang_Trn", "midswi_Pel_Ang_Trn",
        "ic_Hip_Ang_Trn", "ofo_Hip_Ang_Trn", "ofc_Hip_Ang_Trn", "fo_Hip_Ang_Trn", "midswi_Hip_Ang_Trn",
        "ic_Foo_Ang_Trn", "ofo_Foo_Ang_Trn", "ofc_Foo_Ang_Trn", "fo_Foo_Ang_Trn", "midswi_Foo_Ang_Trn"
      )  
  
  if(v == "FAQT") pred_vars <- 
      c("FAQT",
        "avgspa"
      )  
  
  # Anteversion and Related Measures -----
  vgoals <- c("TOTAL_Score", "Activities_Sports_Rec", "ADL_Indep", "Braces_Mobility",
              "Gait_Func_Mobility", "Gait_Pattern_Appearance", "Image_Esteem", 
              "Pain_Discomfort_Fatigue")

  if(v %in% vgoals) pred_vars <- 
      c(vgoals, 
        
        "GDI", "FAQT", "DMC", "avgspa", "avgstr", "avgsel", 
        "avgwb", "avgnwb",
        "HIP_INT_ROT", "HIP_EXT_ROT", "ANTEVERSION", "EOS_FemTor", "BIMAL", "EOS_Bimal",
        "HIP_EXT", "POP_ANG_UNI",
        "KNEE_EXT", "EXTEN_LAG", "PATELLA_ALTA",
        "ANK_DORS_0", "ANK_DORS_90",
        
        "ic_Pel_Ang_Sag", "midsta_Pel_Ang_Sag", "ofc_Pel_Ang_Sag", "fo_Pel_Ang_Sag", "midswi_Pel_Ang_Sag",
        "ic_Hip_Ang_Sag", "midsta_Hip_Ang_Sag", "ofc_Hip_Ang_Sag", "fo_Hip_Ang_Sag", "midswi_Hip_Ang_Sag",
        "ic_Kne_Ang_Sag", "midsta_Kne_Ang_Sag", "ofc_Kne_Ang_Sag", "fo_Kne_Ang_Sag", "midswi_Kne_Ang_Sag",
        "ic_Ank_Ang_Sag", "midsta_Ank_Ang_Sag", "ofc_Ank_Ang_Sag", "fo_Ank_Ang_Sag", "midswi_Ank_Ang_Sag",
        
        "ic_Pel_Ang_Cor", "midsta_Pel_Ang_Cor", "ofc_Pel_Ang_Cor", "fo_Pel_Ang_Cor", "midswi_Pel_Ang_Cor",
        "ic_Hip_Ang_Cor", "midsta_Hip_Ang_Cor", "ofc_Hip_Ang_Cor", "fo_Hip_Ang_Cor", "midswi_Hip_Ang_Cor",
        
        "ic_Pel_Ang_Trn", "midsta_Pel_Ang_Trn", "ofc_Pel_Ang_Trn", "fo_Pel_Ang_Trn", "midswi_Pel_Ang_Trn",
        "ic_Hip_Ang_Trn", "midsta_Hip_Ang_Trn", "ofc_Hip_Ang_Trn", "fo_Hip_Ang_Trn", "midswi_Hip_Ang_Trn",
        "ic_Foo_Ang_Trn", "midsta_Foo_Ang_Trn", "ofc_Foo_Ang_Trn", "fo_Foo_Ang_Trn", "midswi_Foo_Ang_Trn"
      )
  
  return(pred_vars)
}
