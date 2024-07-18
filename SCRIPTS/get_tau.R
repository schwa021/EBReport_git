# Function to build outcome summary table -----

get_tau <- function(out_all, surglist){
  
  # Compute treatment effect (tau) for one surgery, one side, one variable -----
  compute_tau <- function(out_all, s, sd, vv){
    temp <- out_all %>% 
      filter(
        surgname == s,
        side == sd,
        var == vv
      ) %>% 
      select(surg, pct50)
    tau <- temp$pct50[temp$surg == "Treated"] - temp$pct50[temp$surg == "Control"]
    return(tau)
  }
  
  # Compute all tau for all surgery for all outcomes for each side -----
  vall <- unique(out_all$var)
  tempL <- list()
  tempR <- list()
  for (s in surglist) {
    tempL[[s]] <- vall %>% map_vec(\(vv) compute_tau(s, "L", vv))
    tempR[[s]] <- vall %>% map_vec(\(vv) compute_tau(s, "R", vv))
  }
  
  maketau <- function(temp, sd, vall){
    res <- as_tibble(temp)
    res$name <- vall
    res$SIDE <- sd
    res$measure <- "tau"
    res <- relocate(res, c(name, measure, SIDE)) 
    return(res)
  }
  
  # Organize tau -----
  tauL <- maketau(tempL, "L", vall)
  tauR <- maketau(tempR, "R", vall)
  tau <- bind_rows(tauL, tauR)
  
}

