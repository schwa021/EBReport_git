# ---- Get threshold for outcome variables with unknown direction ----
get_outcome_vunk <- 
  function(df, vv){
    
    temp <- get_outcome_thresh(vv, df[1,])[1]
    
    # Guess signs for BIMAL ----
    if(vv == "BIMAL"){
      sgn <- case_when(
        df$BIMAL > 30 ~ -1,
        df$BIMAL < 0 ~ 1,
        TRUE ~ 1
      )
    }
    
    # Guess signs for ANTEVERSION ----
    if(vv == "ANTEVERSION"){
      sgn <- case_when(
        df$ANTEVERSION < 10 ~ 1,
        TRUE ~ -1
      )
    }
    
    # Guess signs for Kne_Ang_Trn ----
    if(vv == "meansta_Kne_Ang_Trn"){
      sgn <- case_when(
        df$meansta_Kne_Ang_Trn > 8 ~ -1,
        df$meansta_Kne_Ang_Trn < -27 ~ 1,
        TRUE ~ 1
      )
    }
    
    # Guess signs for Hip_Ang_Trn ----
    if(vv == "meansta_Hip_Ang_Trn"){
      sgn <- case_when(
        df$meansta_Hip_Ang_Trn < -13 ~ 1,
        TRUE ~ -1
      )
    }
    
    # Guess signs for Kne_Ang_Trn ----
    if(vv == "meansta_Foo_Ang_Trn"){
      sgn <- case_when(
        df$meansta_Foo_Ang_Trn > 11 ~ -1,
        df$meansta_Foo_Ang_Trn < -31 ~ 1,
        TRUE ~ -1
      )
    }
    
    thresh <- temp * sgn
  }
