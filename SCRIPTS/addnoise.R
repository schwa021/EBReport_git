addnoise <- function(x, N, vbls){
  
  # Deformity measures from PE -----
  DFMerr <- c("HIP_FLEX" = 7.5, "HIP_EXT" = 5,
              "HIP_ABD_0" = 5,  "HIP_ABD_90" = 5, 
              "HIP_EXT_ROT" = 6, "HIP_INT_ROT" = 6,
              "POP_ANG_UNI" = 8, "POP_ANG_BI" = 9,
              "KNEE_FLEX" = 9, "KNEE_EXT" = 3, "EXTEN_LAG" = 5,
              "ANK_DORS_0" = 6, "ANK_DORS_90" = 6, "PLANT_FLEX" = 6,
              "ANTEVERSION" = 7.5, "BIMAL" = 5,
              "EOS_FemTor" = 5, "EOS_Bimal" = 5,
              "HEIGHT" = 1, "WEIGHT" = .25)
  
  # STR, SEL, SPAS from Boyer (2022) -----
  strprob <- matrix(c(
    60, 20, 15, 05, 00, 00, 00,
    15, 60, 15, 10, 00, 00, 00,
    05, 15, 60, 15, 05, 00, 00,
    00, 05, 15, 60, 15, 05, 00,
    00, 00, 10, 15, 60, 15, 00,
    00, 00, 05, 15, 20, 60, 00,
    100/6, 100/6, 100/6, 100/6, 100/6, 100/6, 00),
    nrow = 7, ncol = 7, byrow = T)
  
  selprob <- matrix(c(
    65, 25, 10, 00,
    18, 65, 17, 00,
    10, 25, 65, 00,
    100/3, 100/3, 100/3, 00),
    nrow = 4, ncol = 4, byrow = T)
  
  # Note: currently no data for SPA so make some up
  spaprob <- matrix(c(
    60, 30, 10, 00, 00, 00,
    20, 50, 20, 10, 00, 00,
    05, 20, 50, 20, 05, 00,
    00, 10, 20, 50, 20, 00,
    00, 00, 10, 30, 60, 00,
    20, 20, 20, 20, 20, 00),
    nrow = 6, ncol = 6, byrow = T)
  
  # Note: currently no data for GMFCS so make some up
  GMFCSprob <- matrix(c(
    75, 25, 05, 00, 00, 00,
    15, 70, 15, 00, 00, 00,
    05, 20, 75, 00, 00, 00,
    00, 05, 20, 75, 00, 00,
    00, 00, 00, 00, 25, 75,
    00, 00, 00, 00, 00, 100),
    nrow = 6, ncol = 6, byrow = T)
  
  # Note: currently no data for FAQ so make some up
  FAQprob <- matrix(c(
    70, 20, 05, 00, 00, 00, 00, 00, 00, 00, 00,
    15, 65, 15, 05, 00, 00, 00, 00, 00, 00, 00,
    05, 15, 60, 15, 05, 00, 00, 00, 00, 00, 00,
    00, 05, 15, 60, 15, 05, 00, 00, 00, 00, 00,
    00, 00, 05, 15, 60, 15, 05, 00, 00, 00, 00,
    00, 00, 00, 05, 15, 60, 15, 05, 00, 00, 00,
    00, 00, 00, 00, 05, 15, 60, 15, 05, 00, 00,
    00, 00, 00, 00, 00, 05, 15, 60, 15, 05, 00,
    00, 00, 00, 00, 00, 00, 05, 15, 65, 15, 00,
    00, 00, 00, 00, 00, 00, 00, 10, 20, 70 ,00,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 00),
  nrow = 11, ncol = 11, byrow = T)
  
   
  # Lists of variables in groups
  fixedvars <- c("SIDE", "age", "dx", "dxmod", "Sex")
  gaitvars <- str_subset(vbls, "^mean|^min|^max|^rom|^fo|^t_|^ic|footoff|footcontact")
  strvars <- str_subset(vbls, "_STR$")
  selvars <- str_subset(vbls, "_SEL$")
  spavars <- str_subset(vbls, "_SPAS$")
  surgvars <- str_subset(vbls, "^interval|^prior")
  dfmvars <- c( "ANTEVERSION", "HIP_INT_ROT", "HIP_EXT_ROT", "BIMAL", "HIP_EXT", "POP_ANG_UNI", "KNEE_EXT", "EXTEN_LAG", "ANK_DORS_0", "ANK_DORS_90", "HIP_ABD_0", "HEIGHT", "WEIGHT")
  footvars <- str_subset(vbls, "^WB|^NWB")
  
  # Add noise to categorical variable -----
  catprob <- function(v, prob){
    set.seed(42)
    irow <- vector()
    for (kk in 1:length(v)) {
      irow[kk] <- which(v[kk] == levels(v))
    }
    p <- prob[irow, ]
    # Note: rcat is the key - samples from categorical distr
    vrep <- rcat(length(v), p)
    vrepfact <- levels(v)[vrep]
    return(vrepfact)
  } 
  
  # Add noise to scale variable -----
  sclprob <- function(A, B, v){
    set.seed(42)
    A[[v]] <- A[[v]] + rnorm(nrow(A), 0, B[[v]])
    return(A)
  }
  
  # Add noise to gait variable -----
  gaitprob <- function(z, errdat, v){
    set.seed(42)
    z[[v]] <- z[[v]] + rnorm(nrow(z), 0, errdat[[v]])
    return(z)
  } 
  
  # Generate N replicates of x -----
  xx <- x[rep(seq_len(nrow(x)), each = N), ]
  
  # Add noise to SEL, STR, SPAS, GMFCS, FAQ, gait data -----
  # Note: gait data currently kludge... 3 for min, max, mean, ...
  set.seed(42)
  zz <- xx %>%
    select(all_of(vbls)) %>%
    mutate(across(all_of(selvars), ~ catprob(., selprob))) %>%     
    mutate(across(all_of(spavars), ~ catprob(., spaprob))) %>% 
    mutate(across(all_of(strvars), ~ catprob(., strprob))) %>% 
    mutate(across(all_of(footvars), ~ .)) %>% 
    mutate(GMFCS = catprob(GMFCS, GMFCSprob)) %>% 
    mutate(FAQ = catprob(FAQ, FAQprob)) %>% 
    mutate(across(matches("^min|^max|^mean|^ic_[PHKAF]|^fo_|^ofo_|^t_"), 
                  ~ . + rnorm(nrow(xx), 0, 3))) %>% 
    mutate(across(matches("PEL"), ~ . + rnorm(nrow(xx), 0, 7.5))) %>% 
    mutate(across(matches("^rom"), ~ . + rnorm(nrow(xx), 0, 1.5))) %>% 
    mutate(across(matches("^GDI|^DMC"), ~ . + rnorm(nrow(xx), 0, 5)))

  # Add noise to deformity (e.g., ANTEVERSION, HIP_EXT) -----
  for (v in dfmvars) {
    zz <- gaitprob(zz, DFMerr, v) 
  } 
  
  return(zz)
  
}

