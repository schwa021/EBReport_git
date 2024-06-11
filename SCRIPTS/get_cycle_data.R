# Get data for a cycle --------------------------------------------------------
# Take the point data (ptdat), column names (vnames) and a cycle number (cycle)
# Returns a tibble of left and right data spline fit to 51 points over the cycle
get_cycle_data <- function(c, vnames, Lcyc, Rcyc, typestr, cycle){
  
  # Spline fit data to 51 points and return y-values
  makespline <- function(x){
    if(sum(!is.na(x)) == 0){
      s <- rep(NA, 51)
    } else {
      s <- spline(x, n = 51)
      return(s$y)
    }
  }
  
  # Check for all-zeros - may need to tweak thresholds by typestr --------------
  zero_data <- function(x, typestr){
    if(typestr == "Reaction"){
      if(quantile(abs(x), probs = .90) < .15) x <- rep(NA, length(x))
      return(x) 
    } else if(typestr == "Moment" | typestr == "Power") {
      if(quantile(abs(x), probs = .90) < .02) x <- rep(NA, length(x))
      return(x) 
    } else {
      if(quantile(abs(x), probs = .90) < .15) x <- rep(NA, length(x))
      return(x)
    }
  }
  
  # Function to get data -------------------------------------------------------
  get_d <- function(dat, side, Scyc, Sk, cycle, value_scale){
    
    oppside <- ifelse(side == "L", "R", "L")
    
    d <- dat %>% 
      filter(between(Frame, Scyc$FC1[cycle], Scyc$FC2[cycle])) %>% 
      select(-Frame) %>% 
      select(-starts_with(oppside)) %>% 
      map_df(makespline) %>% 
      rename_with(~ str_replace(.x, side, "")) %>% 
      mutate(
        side = side,
        cycle = cycle,
        OFO = Sk$OFO[cycle],
        OFC = Sk$OFC[cycle],
        FO = Sk$FO[cycle],
        speed = mean(speed),
        steplen = abs(disp[51] - disp[Sk$OFC[cycle]]),
        stridelen = abs(disp[51] - disp[1]),
        oppsteplen = stridelen - steplen
      ) %>% 
      select(-disp) %>% 
      mutate(t = seq(0, 100, length = 51)) %>% 
      mutate(speed = ifelse(speed < 0, NA, speed)) %>% 
      mutate(across(-c(side, cycle, OFO, OFC, FO, steplen, stridelen, oppsteplen, speed, t),
                    ~ . / value_scale)) %>% 
      mutate(across(-c(side, cycle, OFO, OFC, FO, steplen, stridelen, oppsteplen, speed, t), 
                    ~ zero_data(., typestr))) %>% 
      relocate(speed, .after = steplen)
    
    return(d)
  }
  
  
  # Extract point data ---------------------------------------------------------
  ptdat <- c$VideoData
  vidrate <- c$Header$Video_Sampling_Rate
  
  
  # If no pelvic origin - return empty result ----------------------------------
  pelx <- any(str_detect(names(ptdat), "^PELO"))
  if(!pelx){
    d <- tibble()
    return(d)
  }
  
  
  # Get scale factors ----------------------------------------------------------
  param_name <- case_when(
    typestr == "Moment" ~ "MOMENT_UNITS",
    typestr == "Power" ~ "POWER_UNITS",
    TRUE ~ "UNITS"
  )
  
  # Distance scale
  dist_units <- c$Parameters %>% 
    filter(GroupName == "POINT") %>% 
    filter(ParameterName == "UNITS") %>% 
    .$Data %>% 
    unlist() %>% 
    str_split_1("[ ]{2,}")
  
  dist_scale <- case_when(
    dist_units == "mm" ~ 1000,
    dist_units == "cm" ~ 100,
    dist_units == "dm" ~ 10,
    dist_units == "km" ~ .001,
    TRUE ~ 1
  )
  
  # Data scale
  value_units <- c$Parameters %>% 
    filter(GroupName == "POINT") %>% 
    filter(ParameterName == param_name) %>% 
    .$Data %>% 
    unlist() %>% 
    str_split_1("[ ]{2,}")
  
  value_scale <- case_when(
    value_units == "mm" | value_units == "Nmm" | value_units == "mW" ~ 1000,
    value_units == "cm" | value_units == "Ncm" ~ 100,
    value_units == "dm" | value_units == "Ndm" ~ 10,
    value_units == "km" | value_units == "Nkm" ~ .001,
    TRUE ~ 1
  )
  
  if(typestr %in% c("Angle", "Trunk", "Length")) value_scale = 1
  
  
  # get speed and steplength ---------------------------------------------------
  # Clumsy *ANK selection d/t files where trajectory appears more than once
  lank <- ptdat[str_detect(names(ptdat), "LANK")][,1:3]
  rank <- ptdat[str_detect(names(ptdat), "RANK")][,1:3]
  
  # Get pelvic origin
  pelvis <- 
    ptdat %>% 
    select(str_subset(names(ptdat), "^PELO"))
  
  # confirm direction of travel
  # rom <- apply(lank, 2, range)
  rom <- apply(pelvis, 2, range)
  rom <- abs(rom[2,] - rom[1,])
  lank <- lank[, which.max(rom)][[1]] / dist_scale
  rank <- rank[, which.max(rom)][[1]] / dist_scale
  pelvis <- pelvis[, which.max(rom)][[1]] / dist_scale
  
  # Get direction by fitting line to lank (exclude 0)
  temp <- lank[lank != 0]
  temp <- pelvis[pelvis != 0]
  nt <- length(temp)
  n1 <- round(.25*nt)
  n2 <- round(.75*nt)
  dir <- sign(sum(temp[(n1+1):n2] - temp[n1:(n2 - 1)]))
  
  
  # Spline fit displacement, derive speed --------------------------------------
  x <- (1:length(lank))/c$Header$Video_Sampling_Rate
  lanks <- splinefun(x, lank)
  ranks <- splinefun(x, rank)
  Ldisp <- lanks(x, deriv = 0)
  Lspeed <- dir * lanks(x, deriv = 1)    # note: dir corrects for "backwards" walk
  Rdisp <- ranks(x, deriv = 0)
  Rspeed <- dir * ranks(x, deriv = 1)    # note: dir corrects for "backwards" walk
  
  pelspline <- splinefun(x, pelvis)
  disp <- pelspline(x, deriv = 0)
  speed <- pelspline(x, deriv = 1)
  
  
  # Add chirality to vnames ----------------------------------------------------
  vL <- glue("L{vnames}")
  vR <- glue("R{vnames}")
  vLR <- c(vL, vR)
  
  
  # Extract variables (vLR), speed, and displacement -----------------------
  dat <- ptdat[, c("Frame", vLR)]
  # dat <- cbind(dat, Ldisp, Rdisp, Lspeed, Rspeed)
  dat <- cbind(dat, disp, speed)
  
  
  # Transform events to pct cycle time (i.e. "t" variable) ---------------------
  Lk <- round((100 * (Lcyc - Lcyc$FC1) / (Lcyc$FC2 - Lcyc$FC1))) + 1
  Rk <- round((100 * (Rcyc - Rcyc$FC1) / (Rcyc$FC2 - Rcyc$FC1))) + 1
  
  
  # If frames span events, extract data, else return empty tibble --------------
  if(first(ptdat$Frame) > min(Lcyc, Rcyc) | last(ptdat$Frame) < max(Lcyc, Rcyc)){
    d <- tibble()
  } else {
    dL <-  get_d(dat, side = "L", Scyc = Lcyc, Sk = Lk, cycle, value_scale)
    dR <-  get_d(dat, side = "R", Scyc = Rcyc, Sk = Rk, cycle, value_scale)
    d <- bind_rows(dL, dR)
  }
  
  return(d)
  
}
