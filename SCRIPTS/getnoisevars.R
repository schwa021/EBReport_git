getnoisevars <- function(x){
  # Choose Predictors -----
  varstr <- c(
    "^age", "^dx", "^dxmod", "^Sex",
    "GMFCS$", "FAQ$", "FAQT$",
    "GDI", "DMC",
    "^HEIGHT", "^WEIGHT",
    "EOS_FemTor", "EOS_Bimal",
    "ANTEVERSION", "HIP_INT_ROT", "HIP_EXT_ROT", "BIMAL",
    "HIP_EXT", "POP_ANG_UNI", "KNEE_EXT", "EXTEN_LAG", "ANK_DORS_0", "ANK_DORS_90",
    "HIP_ABD_0", "HIP_ABD_90", "PATELLA_ALTA",
    "STR$", "SPAS$", "SEL$",
    "^WB", "^NWB",
    "^mean", "^min", "^max", "^rom", "^fo", "^t_", "^ic",
    "footoff", "footcontact"
  )
  
  vars <- str_subset(names(x %>% select(!ends_with("Post"))), paste(varstr, collapse = "|"))
  vars <- str_subset(vars, "Mom|Pwr|len|vel", negate = T)
  vars <- str_subset(vars, "_L$|_R$|_Conv$", negate = T)
  vars <- str_subset(vars, "NA", negate = T)
  
  # Get prior treatment -----
  prior <- str_subset(names(x), "prior_")
  prior <-
    str_subset(prior, "Neurolysis|Cast|Other|Unknown|Hardware|Upper|BOTOX|Leg_Length|Exploratory|Combo|TKA|Spine|Baclofen|FDO_DFEO|Pelvic", negate = T)
  prior <- str_subset(prior, "Post", negate = T)
  
  
  # Get interval treatment -----
  interval <- str_subset(names(x), "interval_")
  interval <-
    str_subset(interval, "Neurolysis|Cast|Other|Unknown|Hardware|Upper|BOTOX|Leg_Length|Exploratory|Combo|TKA|Spine|Baclofen|FDO_DFEO|Pelvic", negate = T)
  interval <- str_subset(interval, "Post", negate = T)
  
  # Get predictor variables -----
  allvars <- c(vars, prior)
}