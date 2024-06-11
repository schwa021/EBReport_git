get_TD_NEW <- function() {
  td <- read_csv("DATA/FMCPAT_Control_Angle.csv")
  
   # Fix names --------------------------------------------------------------------
  temp <- str_split_fixed(td$Row, "_", 2)
  var <- temp[,1]
  t <- temp[,2]
  
  var <- glue("Ang_{var}")
  var <- str_replace_all(var, "Tra", "_Trn")
  var <- str_replace_all(var, "Sag", "_Sag")
  var <- str_replace_all(var, "Cor", "_Cor")
  var <- str_replace_all(var, "Knee", "Kne")
  var <- str_replace_all(var, "Pelvis", "Pel")
  var <- str_replace_all(var, "Ankle", "Ank")
  var <- str_replace_all(var, "FootProgress", "Foo")
  var <- str_replace_all(var, "Trunk", "Trk")
  var <- str_sub(var, 5, 11)
  var <- str_replace(var, "_", "_Ang_")
  td$var <- var
  td$t <- t
  td <- 
    td %>% 
    mutate(Row = NULL) %>% 
    relocate(c(var, t)) %>% 
    mutate(t = 2*(as.numeric(t)-1)) |> 
    rename(
      value = Mean,
      sd = STD
      ) |> 
    mutate(
      lwr = value - 2 * sd,
      upr = value + 2 * sd,
      name = str_replace_all(var, "_", "."),
      var = NULL,
      type = "TD"
    )
  
  return(td)
}