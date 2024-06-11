add_features <- function(FD, datc3d){
  
  # Function to get features from average data ---------------------------------
  get_features <- function(d, nscores, prefix){
    # Make long -----
    dd <- list_rbind(d)
    
    # Remove transverse knee angles from < 2006 -----
    irow <- dd$Year < 2006 & str_detect(dd$name, "Kne.Ang.Trn")
    dd$value[irow] <- NA
    
    # Remove Ankle coronal and foot coronal/sagittal angles -----
    ddw <- 
      dd %>% 
      select(side, t, name, Exam_ID, value) %>%
      
      # Added to handle case with multiple BF conditions (e.g., +/- walker)
      distinct() |> 
      
      pivot_wider(names_from = c(side, Exam_ID), values_from = value) %>% 
      arrange(name, t)
    
    # Get matrix of values -----
    x <- 
      ddw %>% 
      filter(!str_detect(name, "Ank.Ang.Cor|Foo.Ang.Cor|Foo.Ang.Sag")) %>%
      select(starts_with(c("L", "R"))) %>% 
      as.matrix()
    
    # Replace missing values with mean over all subjects -----
    narepl <- function(x, rmean){
      x[is.na(x)] <- rmean[is.na(x)]
      return(x)
    }
    rmean <- rowMeans(x, na.rm = T)
    x <- apply(x, 2, FUN = narepl, rmean=rmean)
    
    # Get all scores -----
    res <- svd(x, nu=nscores)
    scores <- as_tibble(t(res$u)[1:nscores,] %*% x)
    
    # Organize and return nscores scores -----
    s <- 
      scores %>% 
      mutate(sname = glue("Feat_{prefix}_{row_number()}")) %>% 
      relocate(sname) %>% 
      pivot_longer(-sname, names_to = c("side", "Exam_ID"), names_sep = "_") %>% 
      pivot_wider(names_from = sname, values_from = value)
    
    return(s)
  }
  
  # Get features -----
  F_ang <- get_features(datc3d$Ang_avg, 20, "Ang")
  if(!is.null(nrow(datc3d$Mom_avg))){
    F_mom <- get_features(datc3d$Mom_avg, 20, "Mom")
    F_pwr <- get_features(datc3d$Pwr_avg, 20, "Pwr")
    FD <- 
      FD %>% 
      left_join(F_ang, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) %>% 
      left_join(F_mom, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID")) %>%
      left_join(F_pwr, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  } else{
    FD <- 
      FD %>% 
      left_join(F_ang, by = c("SIDE" = "side", "Exam_ID" = "Exam_ID"))
  }

  
  # Return updated FD -----
  return(FD)
}