
# Adjustment set for direct effect of age -----
vadj <- c(
  "age",
  "ANK_DORS_0", "ANTEVERSION", "BIMAL", "DMC", "EXTEN_LAG", "HIP_ABD_0", 
  "HIP_EXT", "HIP_FLEX", "KNEE_EXT", "KNEE_FLEX", "NDsteplen", "NDspeed",
  "NDsteplen", "NETND_OXYCONS_Z", "PLANT_FLEX", "POP_ANG_UNI", "WB_4FTPOS",
  "WB_4FTPOS2", "WB_FTPOS", "WB_MIDFT_POS", "dx", "FAQT", "ic_Ank_Ang_Sag", 
  "ic_Kne_Ang_Sag", "maxswi_Kne_Ang_Sag", "meansta_Pel_Ang_Sag", "avgsel", "avgspa",
  "avgstr", "meansta_Ank_Ang_Sag", "meansta_Foo_Ang_Trn", "meansta_Hip_Ang_Trn",
  "meansta_Kne_Ang_Trn", "meanswi_Ank_Ang_Sag", "minsta_Hip_Ang_Sag",
  "minsta_Kne_Ang_Sag", "rom_Pel_Ang_Sag", "romswi_Kne_Ang_Sag"
)

temp <- 
  datpre %>% 
  select(all_of(vadj), TOTAL_Score) %>% 
  data.frame() %>% 
  drop_na(TOTAL_Score)

x <- temp %>% select(all_of(vadj)) %>% data.frame()
y <- temp %>% pull(TOTAL_Score)

mod <- bartMachine(x, y, use_missing_data = TRUE, serialize = TRUE)
