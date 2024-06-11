get_outcome_vars <- function(s){
  
  # Derotation Osteotomies -----
  if(s == "Femoral_Derotation_Osteotomy") out_vars <- 
      c("ANTEVERSION", "meansta_Hip_Ang_Trn", "GDI", "FAQT")
  
  if(s == "Tibial_Derotation_Osteotomy") out_vars <- 
      c("BIMAL", "meansta_Kne_Ang_Trn", "GDI", "FAQT")
  
  
  # Muscle Lengthening -----
  if(s == "Psoas_Release") out_vars <- 
      c("HIP_EXT", "minsta_Hip_Ang_Sag", "GDI", "FAQT")
  
  if(s == "Hams_Lengthening") out_vars <- 
      c("POP_ANG_UNI", "ic_Kne_Ang_Sag", "GDI", "FAQT")
  
  if(s == "Gastroc_Soleus_Lengthening") out_vars <- 
      c("ANK_DORS_0", "ic_Ank_Ang_Sag", "GDI", "FAQT")
  
  if(s == "Adductor_Release") out_vars <- 
      c("HIP_ABD_0", "minswi_Hip_Ang_Cor", "GDI", "FAQT")
  
  
  # Crouch -----
  if(s == "Patellar_Advance") out_vars <- 
      c("EXTEN_LAG", "minsta_Kne_Ang_Sag", "GDI", "FAQT")
  
  if(s == "DFEO") out_vars <- 
      c("KNEE_EXT", "minsta_Kne_Ang_Sag", "GDI", "FAQT")
  
  if(s == "DFEO_Patellar_Advance") out_vars <- 
      c("KNEE_EXT", "minsta_Kne_Ang_Sag", "GDI", "FAQT")  
  
  # Spasticity -----
  if(s == "Neural_Rhizotomy") out_vars <- 
      c("avgspa", "meansta_Ank_Ang_Sag", "GDI", "FAQT")
  
  if(s == "Rectus_Transfer") out_vars <- 
      c("KNEE_FLEX", "maxswi_Kne_Ang_Sag", "GDI", "FAQT")
  
  
  # Foot and Ankle -----
  if(s == "Foot_and_Ankle_Bone") out_vars <- 
      c("avgwb", "meansta_Foo_Ang_Trn", "GDI", "FAQT")
  
  if(s == "Foot_and_Ankle_Soft_Tissue") out_vars <- 
      c("avgnwb", "meansta_Foo_Ang_Trn", "GDI", "FAQT")
  
   return(out_vars)
  
}
