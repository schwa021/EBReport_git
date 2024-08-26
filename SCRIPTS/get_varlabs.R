get_varlabs <- function(){
  # Get labels
  varlabs <- read_csv(
    "C:/Users/mschwartz/OneDrive - Gillette Children's Specialty Healthcare/R RFMP/R RFMP/RFMP_all/DATA/variable_labels.csv"
  )
  
  # Fix gait data labels
  gvar <- str_subset(varlabs$Variable, "^ic|^fo|^ofo|^ofc|^mean|^min|^max|^t_|^mids")
  
  gvarlab <- 
    gvar %>%
    str_replace("^t", "Time of") %>% 
    str_replace("^ofo", "Opposite Foot Off") %>% 
    str_replace("^ofc", "Opposite Foot Contact") %>% 
    str_replace("^ic", "Initial Contact") %>% 
    str_replace("^fo", "Foot Off") %>% 
    str_replace("midsta", "Mid-Stance") %>%
    str_replace("midswi", "Mid-Swing") %>%
    str_replace("mean", "Mean ") %>% 
    str_replace("min", "Minimum ") %>% 
    str_replace("max", "Maximum ") %>% 
    str_replace("sta", "Stance ") %>% 
    str_replace("swi", "Swing ") %>% 
    str_replace("Pel", "Pelvis") %>% 
    str_replace("Kne", "Knee") %>% 
    str_replace("Ank", "Ankle") %>% 
    str_replace("Foo_", "Foot_") %>% 
    str_replace("Ang", "Angle") %>% 
    str_replace("Mom", "Moment") %>% 
    str_replace("Pwr", "Power") %>% 
    str_replace("len", "Length") %>% 
    str_replace("vel", "Strain Rate") %>% 
    str_replace_all("_", " ") %>% 
    str_replace_all("  ", " ") %>% 
    str_replace("Add Long", "Adductor Longus") %>% 
    str_replace("Med Gas", "Medial Gastrocnemius") %>% 
    str_replace("Semimem", "Semimembranosus") %>% 
    str_replace("Rect Fem", "Rectus Femoris") %>% 
    str_replace("Cor", "Coronal Plane") %>% 
    str_replace("Sag", "Sagittal Plane") %>% 
    str_replace("Trn", "Transverse Plane")
  
  gvarlabx <- glue("\U1F6B6\U1F3FD {gvarlab}")
  
  temp <-
    tibble(
      Variable = gvar,
      Label = gvarlab,
      Labelx = as.character(gvarlabx)
    )
  
  varlabs <- 
    varlabs %>% 
    left_join(temp, by = "Variable") %>% 
    mutate(Label = ifelse(!is.na(Label.y), Label.y, Label.x)) %>% 
    select(Variable, Label, Labelx, negdir, posdir)
  
  # Fill in direction labels -----
  # Angles and time
  varlabs <- 
    varlabs |> 
    mutate(
      negdir = case_when(
        str_detect(Variable, "^(?!t).*Pel_Ang_Sag") ~ "Extension",
        str_detect(Variable, "^(?!t).*Pel_Ang_Cor") ~ "Down",
        str_detect(Variable, "^(?!t).*Pel_Ang_Trn") ~ "External",
        str_detect(Variable, "^(?!t).*Hip_Ang_Sag") ~ "Extension",
        str_detect(Variable, "^(?!t).*Hip_Ang_Cor") ~ "Abduction",
        str_detect(Variable, "^(?!t).*Hip_Ang_Trn") ~ "External",
        str_detect(Variable, "^(?!t).*Kne_Ang_Sag") ~ "Extension",
        str_detect(Variable, "^(?!t).*Kne_Ang_Cor") ~ "Valgus",
        str_detect(Variable, "^(?!t).*Kne_Ang_Trn") ~ "External",
        str_detect(Variable, "^(?!t).*Ank_Ang_Sag") ~ "Plantarflexion",
        str_detect(Variable, "^(?!t).*Ank_Ang_Cor") ~ "Eversion",
        str_detect(Variable, "^(?!t).*Ank_Ang_Trn") ~ "External",
        str_detect(Variable, "^(?!t).*Foo_Ang_Sag") ~ "Equinus",
        str_detect(Variable, "^(?!t).*Foo_Ang_Cor") ~ "Eversion",
        str_detect(Variable, "^(?!t).*Foo_Ang_Trn") ~ "Out",
        str_detect(Variable, "^t_") ~ "Earlier",
        str_detect(Variable, "Pwr$") ~ "Absorption",
        str_detect(Variable, "_len$") ~ "Shorter",
        str_detect(Variable, "_vel$") ~ "Slower",
        TRUE ~ negdir
      ),
      posdir = case_when(
        str_detect(Variable, "^(?!t).*Pel_Ang_Sag") ~ "Flexion",
        str_detect(Variable, "^(?!t).*Pel_Ang_Cor") ~ "Up",
        str_detect(Variable, "^(?!t).*Pel_Ang_Trn") ~ "Internal",
        str_detect(Variable, "^(?!t).*Hip_Ang_Sag") ~ "Flexion",
        str_detect(Variable, "^(?!t).*Hip_Ang_Cor") ~ "Adduction",
        str_detect(Variable, "^(?!t).*Hip_Ang_Trn") ~ "Internal",
        str_detect(Variable, "^(?!t).*Kne_Ang_Sag") ~ "Flexion",
        str_detect(Variable, "^(?!t).*Kne_Ang_Cor") ~ "Varus",
        str_detect(Variable, "^(?!t).*Kne_Ang_Trn") ~ "Internal",
        str_detect(Variable, "^(?!t).*Ank_Ang_Sag") ~ "Dorsiflexion",
        str_detect(Variable, "^(?!t).*Ank_Ang_Cor") ~ "Inversion",
        str_detect(Variable, "^(?!t).*Ank_Ang_Trn") ~ "Internal",
        str_detect(Variable, "^(?!t).*Foo_Ang_Sag") ~ "Calcaneous",
        str_detect(Variable, "^(?!t).*Foo_Ang_Cor") ~ "Inversion",
        str_detect(Variable, "^(?!t).*Foo_Ang_Trn") ~ "In",
        str_detect(Variable, "^t_") ~ "Later",
        str_detect(Variable, "Pwr$") ~ "Generation",
        str_detect(Variable, "_len$") ~ "Longer",
        str_detect(Variable, "_vel$") ~ "Faster",
        TRUE ~ posdir
      )
    )
  
  # Moment: TODO
  
  
  return(varlabs)
}