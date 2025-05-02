# ---- Function to generate Shapley table for CART ----
prop_shap_table_CART <- 
  function(xpt, s, mod_list){
    smart_round <- function(x, small_digits = 2) {
      case_when(
        abs(x) < 0.001 ~ round(x, 3),
        abs(x) < 0.01 ~ round(x, 2),
        abs(x) < 0.1 ~ round(x, 1),
        is.na(x) ~ NA_real_,
        TRUE ~ round(x, 0)
      )
    }
    
    # Get patient data ----
    obs <- get_patient_data_NB(xpt, s)
    xL <- obs[1,] %>% mutate(across(where(is.numeric), ~ smart_round(.)))
    xR <- obs[2,] %>% mutate(across(where(is.numeric), ~ smart_round(.)))
    
    # Get model and data ----
    mod <- mod_list[[s]]$mod
    df <- mod_list[[s]]$dat_train %>% select(all_of(names(obs)))
    
    # Compute Shapley Values ----
    shapmod <- Predictor$new(mod, data = df)
    set.seed(42)
    shapL <- Shapley$new(shapmod, x.interest = xL, sample.size = 100)
    shapR <- Shapley$new(shapmod, x.interest = xR, sample.size = 100)
    shapL$results <- shapL$results %>% filter(class == 1)
    shapR$results <- shapR$results %>% filter(class == 1)
    
    # Make Labels for shapley table ----
    makelab <- function(shapres, xside){
      for (f in shapres$results$feature) {
        val <- xside[[f]]
        gaitvbl <- str_detect(f, "^ic|^fo|^ofo|^ofc|^mean|^min|^max|^t_|^mids|^rom")
        lenvbl <- str_detect(f, "len$")
        velvbl <- str_detect(f, "vel$")
        if(is.character(val)) {
          val <- val
        } else {
          val <- case_when(
            gaitvbl & !(lenvbl | velvbl) ~ as.character(val),
            lenvbl ~ as.character(val),
            velvbl ~ sprintf("%.1e", val),
            TRUE ~ as.character(val)
          )
        }
        
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
    
    shapL <- makelab(shapres=shapL, xside=xL)
    shapR <- makelab(shapR, xR)
    
    # Arrange data ----
    arrangeshap <- function(shapres) {
      temp <-
        shapres$results %>%
        arrange(desc(phi)) %>%
        mutate(
          feature.value = str_replace(feature.value, "=", " = "),
          feature.value = str_replace_all(feature.value, "_", " ")
        ) %>%
        mutate(feature.value = fct_rev(fct_inorder(feature.value))) %>%
        mutate(label = fct_rev(fct_inorder(label)))
    }
    shapL <- arrangeshap(shapL)
    shapR <- arrangeshap(shapR)
    
    # Make Shapley value tables ----
    tblL <- 
      shapL %>% 
      select(phi, lab, value) %>% 
      mutate(SIDE = "L") %>% 
      arrange(desc(phi)) %>% 
      rownames_to_column(var="imp") %>% 
      mutate(imp = as.numeric(imp)) %>% 
      relocate(phi) %>% 
      mutate(phi = round(phi,2)) %>% 
      rename(phix = phi)
    
    tblR <- 
      shapR %>% 
      select(phi, lab, value) %>% 
      mutate(SIDE = "R") %>% 
      arrange(desc(phi)) %>% 
      rownames_to_column(var="imp") %>% 
      mutate(imp = as.numeric(imp)) %>% 
      relocate(phi) %>% 
      mutate(phi = round(phi,2)) %>% 
      rename(phix = phi)
    
    tblshap <- bind_rows(tblL, tblR) %>% mutate(Surgery = vlabs[s])
    
    return(tblshap)
  } 
