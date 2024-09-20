orthoseverity <- function(x, ptage, ptsex){
  
  # round age to closest 3 -----
  ptage <- 3 * round(ptage/3)
  
  # Get age-based mean -----
  dd <- readxl::read_xlsx("DATA/development_curves.xlsx")
  dd <- 
    dd |> 
    pivot_longer(-c(vbl, sex, age), names_to = c("pct")) |> 
    mutate(name = vlabs[vbl]) |> 
    filter(
      age == ptage,
      pct == "pct50",
      sex == ptsex
      )
  
  orthotdmean <- c(
    HIP_FLEX = 135,
    HIP_ABD_0 = 30,
    HIP_ABD_90 = 65,
    POP_ANG_BI = 40,
    KNEE_FLEX = 135,
    PATELLA_ALTA = 0,
    HIP_INT_ROT = 45,
    HIP_EXT_ROT = 45,
    ANTEVERSION = 15,
    EOS_FemTor = dd |> filter(vbl=="EOS_FemTor") |> pull(value),
    BIMAL = 15,
    SEC_TOE = 10,
    EOS_Bimal = dd |> filter(vbl == "EOS_Bimal") |> pull(value),
    HIP_EXT = -15,
    POP_ANG_UNI = 45,
    KNEE_EXT = -5,
    EXTEN_LAG = 0,
    ANK_DORS_0 = 10,
    ANK_DORS_90 = 15
  )
  
  orthotdthresh <- c(
    HIP_FLEX = 10,
    HIP_ABD_0 = 10,
    HIP_ABD_90 = 10,
    POP_ANG_BI = 10,
    KNEE_FLEX = 10,
    PATELLA_ALTA = .01,
    HIP_INT_ROT = 5,
    HIP_EXT_ROT = 5,
    ANTEVERSION = 10,
    EOS_FemTor = 5,
    BIMAL = 10,
    SEC_TOE = 10,
    EOS_Bimal = 5,
    HIP_EXT = 10,
    POP_ANG_UNI = 10,
    KNEE_EXT = 5,
    EXTEN_LAG = 5,
    ANK_DORS_0 = 5,
    ANK_DORS_90 = 5  
  )
  
  x$Lsev <- NA
  x$Rsev <- NA
  
  for (v in x$name) {
    L <- x |> filter(name == v) |> pull(Left)
    R <- x |> filter(name == v) |> pull(Right)
    td <- orthotdmean[v]
    thresh <- orthotdthresh[v]
    x$Lsev[x$name == v] <- cut(
      abs((L - td)/thresh),
      breaks = c(-999, 1, 2, 3, 999),
      right = TRUE
    )
    x$Rsev[x$name == v] <- cut(
      abs((R - td)/thresh),
      breaks = c(-999, 1, 2, 3, 999),
      right = TRUE
    )
  }
  
  x$Lsev <- factor(x$Lsev, levels = c(1, 2, 3, 4))
  x$Rsev <- factor(x$Rsev, levels = c(1, 2, 3, 4))
  
  return(x)
}

