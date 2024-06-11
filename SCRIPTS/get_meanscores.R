# Compute mean spasticity, WB, and NWB foot severity. Note that we first check
# the polychoric PCA version - which reveals that the coefficients of the various
# measures are nearly equal - thus a simple mean is justified even though we are
# dealing with categorical variables.

# Also - per Gelman: "...usually I think it’s just fine to model a five-point response 
# as a continuous outcome; indeed, that’s what I recommend."

get_meanscores <- function(df, status) {
  # library(psych)
  
  # Spasticity -----
  spamatch <- ifelse(status == "Pre", ".*_SPAS$", ".*_SPASPost$")
  
  dspa <-
    select(df, matches(spamatch)) %>%
    select(matches("ADD|HAM|PLA|REC")) %>%
    mutate(across(everything(),
                  ~ ifelse(. == "Missing" | . == "Other", NA, .))) %>%
    mutate(across(everything(),
                  ~ as.double(.) - 1))
  
  # cspas <-
  #   polychoric(
  #     x = dspa,
  #     smooth = TRUE,
  #     global = FALSE,
  #     na.rm = TRUE,
  #   )
  
  # pspas <- principal(r = cspas$rho, nfactors = 1) # works if you actually give it the matrix
  # spas_scores <- factor.scores(dspas, pspas)$scores
  avgspa <- rowMeans(dspa, na.rm = T)
  
  
  # Strength -----
  strmatch <- ifelse(status == "Pre", ".*_STR$", ".*_STRPost$")
  
  dstr <-
    select(df, matches(strmatch)) %>%
    mutate(across(everything(),
                  ~ ifelse(. == "Missing" | . == "Other", NA, .))) %>%
    mutate(across(everything(),
                  ~ as.double(.)) - 1)
  
  # cstr <-
  #   polychoric(
  #     x = dstr,
  #     smooth = TRUE,
  #     global = FALSE,
  #     na.rm = TRUE,
  #   )
  
  # pstr <- principal(r = cstr$rho, nfactors = 1) # works if you actually give it the matrix
  # str_scores <- factor.scores(dstr, pstr)$scores
  avgstr <- rowMeans(dstr, na.rm = T)
  
  
  # Selectivity -----
  selmatch <- ifelse(status == "Pre", ".*_SEL$", ".*_SELPost$")
  
  dsel <-
    select(df, matches(selmatch)) %>%
    mutate(across(everything(),
                  ~ ifelse(. == "Missing" | . == "Other", NA, .))) %>%
    mutate(across(everything(),
                  ~ as.double(.)) - 1)
  
  # csel <-
  #   polychoric(
  #     x = dsel,
  #     smooth = TRUE,
  #     global = FALSE,
  #     na.rm = TRUE,
  #   )
  
  # psel <- principal(r = csel$rho, nfactors = 1) # works if you actually give it the matrix
  # sel_scores <- factor.scores(dsel, psel)$scores
  avgsel <- rowMeans(dsel, na.rm = T)  
  
  # Weightbearing (WB) foot severity -----
  wbmatch <- ifelse(status == "Pre", "^WB_.*SEV$", "^WB_.*SEVPost$")
  
  dwb <-
    select(df, matches(wbmatch)) %>%
    mutate(across(everything(),
                  ~ fct_relevel(., "NONE"))) %>%
    mutate(across(everything(),
                  ~ ifelse(. == "Missing" | . == "Other", NA, .))) %>%
    mutate(across(everything(),
                  ~ as.double(.) - 1))
  
  # cwb <-
  #   polychoric(
  #     x = dwb,
  #     smooth = TRUE,
  #     global = FALSE,
  #     na.rm = TRUE,
  #   )
  
  # pwb <- principal(r = cwb$rho, nfactors = 1) # works if you actually give it the matrix
  # wb_scores <- factor.scores(dwb, pwb)$scores
  avgwb <- rowMeans(dwb)
  
  
  # Non-Weightbearing (NWB) foot severity -----
  nwbmatch <-
    ifelse(status == "Pre", "^NWB_.*SEV$", "^NWB_.*SEVPost$")
  
  dnwb <-
    select(df, matches(nwbmatch)) %>%
    mutate(across(everything(),
                  ~ fct_relevel(., "NONE"))) %>%
    mutate(across(everything(),
                  ~ ifelse(. == "Missing" | . == "Other", NA, .))) %>%
    mutate(across(everything(),
                  ~ as.double(.) - 1))
  
  # cnwb <-
  #   polychoric(
  #     x = dwb,
  #     smooth = TRUE,
  #     global = FALSE,
  #     na.rm = TRUE,
  #   )
  
  # pnwb <- principal(r = cwb$rho, nfactors = 1) # works if you actually give it the matrix
  # nwb_scores <- factor.scores(dnwb, pnwb)$scores
  avgnwb <- rowMeans(dnwb)
  
  if(status == "Pre"){
    df$avgspa <- avgspa
    df$avgstr <- avgstr
    df$avgsel <- avgsel
    df$avgwb <- avgwb
    df$avgnwb <- avgnwb
  } else{
    df$avgspaPost <- avgspa
    df$avgstrPost <- avgstr
    df$avgselPost <- avgsel
    df$avgwbPost <- avgwb
    df$avgnwbPost <- avgnwb
  }
  
  return(df)
}
