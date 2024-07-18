get_outcome_thresh <- function(vv, xx_side){
  
  # thresh is a 2-vector. 
  # thresh[1] = magnitude, thresh[2] = direction (-1 ~ < 0, 0 ~ both, 1 ~ > 0)
  
  thresh <- 
    case_when(
      vv == "ANTEVERSION" ~ c(10, -1),
      vv == "meansta_Hip_Ang_Trn" ~ c(5, -1),
      vv == "GDI" ~ c(5, 1),
      vv == "FAQT" ~ c(5, 1),
      vv == "BIMAL" ~ c(5, 0),
      vv == "meansta_Kne_Ang_Trn" ~ c(5, 0),
      vv == "HIP_EXT" ~ c(5, -1),
      vv == "minsta_Hip_Ang_Sag" ~ c(5, -1),
      vv == "POP_ANG_UNI" ~ c(5, -1),
      vv == "ic_Kne_Ang_Sag" ~ c(5, -1),
      vv == "ANK_DORS_0" ~ c(5, 1),
      vv == "ic_Ank_Ang_Sag" ~ c(5, 1),
      vv == "HIP_ABD_0" ~ c(5, 1),
      vv == "minswi_Hip_Ang_Cor" ~ c(5, 1),
      vv == "EXTEN_LAG" ~ c(5, -1),
      vv == "minsta_Kne_Ang_Sag" ~ c(5, -1),
      vv == "KNEE_EXT" ~ c(5, -1),
      vv == "meansta_Ank_Ang_Sag" ~ c(5, 1),
      vv == "avgspa" ~ c(.5, -1),
      vv == "KNEE_FLEX" ~ c(5, 1),
      vv == "maxswi_Kne_Ang_Sag" ~ c(5, 1),
      vv == "avgwb" ~ c(.25, -1),
      vv == "avgnwb" ~ c(.25, -1),
      vv == "meansta_Foo_Ang_Trn" ~ c(5, 0),
      vv == "meanswi_Foo_Ang_Trn" ~ c(5, 0)
    )
  
  # Guess signs for BIMAL -----
  if(vv == "BIMAL"){
    thresh[2] <- case_when(
      xx_side$BIMAL > 30 ~ -1,
      xx_side$BIMAL < 0 ~ 1,
      TRUE ~ 0
    )
  }
  
  # Guess signs for ANTEVERSION -----
  if(vv == "ANTEVERSION"){
    thresh[2] <- case_when(
      xx_side$ANTEVERSION < 10 ~ 1,
      TRUE ~ thresh[2]
    )
  }
  
  # Guess signs for Kne_Ang_Trn -----
  if(vv == "meansta_Kne_Ang_Trn"){
    thresh[2] <- case_when(
      xx_side$meansta_Kne_Ang_Trn > 8 ~ -1,
      xx_side$meansta_Kne_Ang_Trn < -27 ~ 1,
      TRUE ~ thresh[2]
    )
  }
  
  # Guess signs for Hip_Ang_Trn -----
  if(vv == "meansta_Hip_Ang_Trn"){
    thresh[2] <- case_when(
      xx_side$meansta_Hip_Ang_Trn < -13 ~ 1,
      TRUE ~ thresh[2]
    )
  }
  
  # Guess signs for Kne_Ang_Trn -----
  if(vv == "meansta_Foo_Ang_Trn"){
    thresh[2] <- case_when(
      xx_side$meansta_Foo_Ang_Trn > 11 ~ -1,
      xx_side$meansta_Foo_Ang_Trn < -31 ~ 1,
      TRUE ~ thresh[2]
    )
  }
  
   return(thresh)
  
}
