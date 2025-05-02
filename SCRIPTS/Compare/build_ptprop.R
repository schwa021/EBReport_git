# ---- Get propensity data for patient ----
build_ptprop <- 
  function(ypt_prob){
    ptprops <- 
      as_tibble(ypt_prob) %>% 
      mutate(SIDE = c("Left", "Right")) %>% 
      pivot_longer(
        cols = -SIDE,
        names_to = "Surgery",
        values_to = "q50"
      ) %>% 
      mutate(
        Surgery = vlabs[Surgery],
        q5 = NA, q25 = NA, q75 = NA, q95 = NA, 
        q50 = round(100 * q50)
      ) %>% 
      relocate(q5, q25, q75, q95, Surgery, SIDE, q50) %>% 
      mutate(Surgery = factor(Surgery, levels = c("Femoral Derotation Osteotomy", "Tibial Derotation Osteotomy", "Psoas Lengthening", "Hamstring Lengthening", "Adductor Lengthening", "Calf Muscle Lengthening", "DFEO + Patellar Advance", "Patellar Advance", "Foot and Ankle Bone", "Foot and Ankle Soft Tissue", "Selective Dorsal Rhizotomy", "Rectus Femoris Transfer"))) %>% 
      arrange(SIDE, Surgery)
    
    return(ptprops)
  }

