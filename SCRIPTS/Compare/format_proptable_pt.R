# ---- Format propensity table for patient ----
format_proptable_pt <- function(ptprops){
  datL <- ptprops %>% filter(SIDE == "Left") %>% mutate(SIDE = "L")
  datR <- ptprops %>% filter(SIDE == "Right") %>% mutate(SIDE = "R")
  proptable <- ptableLR(datL, datR)
  return(proptable)
}
