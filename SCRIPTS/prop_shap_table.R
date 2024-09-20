# This function creates a table of Shapley values for a single observation

prop_shap_table <- function(df, sname, mod, x, varlabs){
  # Color pallette -----
  pal <- diverging_hcl(n = 5, h = c(324, 120), c = 60, l = c(40, 97), power = 1.8)
  
  # Get Left and Right patient data xL/xR -----
  getx <- function(xx, side){
    xx %>% 
      filter(SIDE == side) %>% 
      select(names(mod$X)) %>% 
      data.frame() %>% 
      mutate(
        across(
          where(is.numeric),
          ~ signif(., 2)
        )
      ) 
  }  
  xL <- getx(x, "L")
  xR <- getx(x, "R")
  
  # Compute Shapley Values -----
  # Recently increased iterations to 100 (20SEP2024)
  shapmod <- Predictor$new(mod, data = df[names(mod$X)])
  set.seed(42)
  # tictoc::tic()
  shapL <- Shapley$new(shapmod, x.interest = xL, sample.size = 100)
  shapR <- Shapley$new(shapmod, x.interest = xR, sample.size = 100)
  # tictoc::toc()
  
  # Make Labels for shapley table -----
  makelab <- function(shapres, xside){
    for (f in shapres$results$feature) {
      val <- xside[[f]]
      gaitvbl <- str_detect(f, "^ic|^fo|^ofo|^ofc|^mean|^min|^max|^t_|^mids")
      val <- ifelse(gaitvbl, round(val), as.character(val))
      
      # Add label - first check for emoji version, then plain text, then blank
      lab <- varlabs$Labelx[varlabs$Variable == f]
      lab <- ifelse(is.na(lab), varlabs$Label[varlabs$Variable == f], lab)
      lab <- ifelse(is.na(lab), f, lab)
      lab <- str_replace_all(lab, "_", " ")
      labx <- str_wrap(glue("{lab} = {val}"), 35)
      
      shapres$results$label[shapres$results$feature == f] <- labx
      shapres$results$lab[shapres$results$feature == f] <- lab
      shapres$results$value[shapres$results$feature == f] <- as.character(val)
    }   
    return(shapres)
  }
  shapL <- makelab(shapL, xL)
  shapR <- makelab(shapR, xR)
  
  # Arrange data and scale phi based on threshold -----
  arrangeshap <- function(shapres){
    temp <- 
      shapres$results %>% 
      arrange(desc(phi)) %>% 
      mutate(
        feature.value = str_replace(feature.value, "=", " = "),
        feature.value = str_replace_all(feature.value, "_", " ")
      ) %>% 
      mutate(feature.value = fct_rev(fct_inorder(feature.value))) %>% 
      mutate(label = fct_rev(fct_inorder(label))) %>% 
      mutate(phix = phi * 0.5 / mod$prob_rule_class)
  }
  shapL <- arrangeshap(shapL)
  shapR <- arrangeshap(shapR)
  
  # Make Shapley value tables -----
  tblL <- 
    shapL %>% 
    select(phix, lab, value) %>% 
    mutate(SIDE = "L") %>% 
    arrange(desc(phix)) %>% 
    rownames_to_column(var="imp") %>% 
    mutate(imp = as.numeric(imp)) %>% 
    relocate(phix) %>% 
    mutate(phix = round(phix,2))
  
  tblR <- 
    shapR %>% 
    select(phix, lab, value) %>% 
    mutate(SIDE = "R") %>% 
    arrange(desc(phix)) %>% 
    rownames_to_column(var="imp") %>% 
    mutate(imp = as.numeric(imp)) %>% 
    relocate(phix) %>% 
    mutate(phix = round(phix,2))
  
  tblshap <- bind_rows(tblL, tblR) %>% mutate(Surgery = sname)
  
  return(tblshap)
  
}
