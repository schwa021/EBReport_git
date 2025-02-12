build_surg <- function(tbls){
  
  # Build surgery (treatment) table
  surg <- left_join(tbls$Surgical.tbl, tbls$SurgDefine.tbl)
  surg <- left_join(surg, tbls$Side.tbl)
  surg <- left_join(tbls$Treatment.tbl, surg)
  surg <- left_join(tbls$Patient_Events.tbl, surg)
  surg <- left_join(tbls$Patient.tbl, surg)
  

  # Define Neurolytic Injection meta
  botox <- surg$SurgCode == "Neural BOTOX"
  injection <-
    (surg$SurgCode == "Neural Other" &
       (surg$Proc_Desc %in% c("motor point block", "nerve block", "tone altering medication")))
  surg$SurgCode[botox | injection] <- "Neurolysis Other"
  
  
  # Neural BOTOX details
  surg <- surg %>%
    mutate(
      SurgCode = case_when(
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "adductor",
          "adductor brevis",
          "adductor longus",
          "adductor magnus",
          "obturator nerve",
          "obturator nerve anterior branch",
          "obturator nerve-anterior branch",
          "pectineus"
        ) ~ "Neurolysis Adductor",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "gastroc-soleus",
          "gastroc (lateral head)",
          "gastroc (medial head)",
          "gastrocnemius",
          "posterior tibialis",
          "soleus",
          "tendo Achilles"
        ) ~ "Neurolysis Calf",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "biceps femoris",
          "gracilis",
          "hamstring", 
          "lateral hamstring",
          "medial hamstring",
          "semimembranosus",
          "semitendinosus",
          "sartorius"
        ) ~ "Neurolysis Hamstring",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "iliacus",
          "iliopsoas",
          "psoas"
        ) ~ "Neurolysis Psoas",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "quadriceps",
          "rectus femoris",
          "vastus intermedius",
          "vastus lateralis",
          "vastus medialis"
        ) ~ "Neurolysis Quadriceps",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "peroneals",
          "peroneus brevis",
          "peroneus longus",
          "peroneus tertius"
        ) ~ "Neurolysis Peroneals",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "gluteus maximus",
          "gluteus medius",
          "gluteus minimus"
        ) ~ "Neurolysis Gluteals",
        SurgCode == "Neurolysis Other" & ProcLoc_Desc1 %in% c(
          "adductor hallucis",
          "extensor hallucis longus",
          "flexor hallucis longus",
          "flexor digitorum longus",
          "flexor digitorum brevis",
          "extensor digitorum longus",
          "extensor digitorum",
          "foot",
          "plantar fascia"
        ) ~ "Neurolysis Foot",
        TRUE ~ SurgCode,
      )
    )
  
  
  # Compute age at treatment
  surg <- surg %>% 
    mutate(surgage = as.numeric(difftime(Event_Date, Birth_Date, units = "days"))/365.25) %>% 
    mutate(surgage = ifelse(surgage < 0, NA, surgage))
  
  
  # Compute laterality
  surg <- surg %>%
    mutate(surgL = (Side %in% c("Asymmetrical L", "Bilateral", "Left"))) %>%
    mutate(surgR = (Side %in% c("Asymmetrical R", "Bilateral", "Right"))) %>% 
    mutate(surgL = ifelse(Side == "Unknown" & SurgCode %in% c("Neural Baclofen", "Neural Rhizotomy", "Spine"), 1, surgL)) %>% 
    mutate(surgR = ifelse(Side == "Unknown" & (SurgCode %in% c("Neural Baclofen", "Neural Rhizotomy", "Spine")), 1, surgR)) %>% 
    mutate(Side = case_when(
      surgL & !surgR ~ "L",
      surgR & !surgL ~ "R",
      surgL & surgR ~ "B",
      !surgL & !surgR ~ "U"
    ))
  
  
  # Drop missing SurgCodd
  surg <- surg %>% drop_na(SurgCode)
  
  
  # Add Details
  surg <- 
    surg %>% 
    left_join(tbls$SurgDetails.tbl) %>% 
    left_join(tbls$SurgDetails_lookup.tbl) %>% 
    select(-c(Surgical_Details_ID, Detail_ID, Instructions, Units, Value_Type, Order, IsReported)) %>% 
    pivot_wider(names_from = c(META_Association, Detail, Field_Format_1, Field_Format_2), values_from = Value) %>% 
    select(-NA_NA_NA_NA)
  
  
  ix <- str_starts(names(surg), "Tibial|Femoral|DFEO|Neural|FDO DFEO|Leg Length")
  vnames <- names(surg)[ix]
  vnames <- str_replace_all(vnames, " ", "_")
  vnames <- str_replace_all(vnames, "\\/", "")
  vnames <- str_remove_all(vnames, "ID|NA|_Direction")
  vnames <- str_remove_all(vnames, "_$|__$|___$")
  vnames <- str_remove_all(vnames, "_Physician")
  
  vnames <- str_remove_all(vnames, "Presurg_Torsion_|Presurg_FlxExt_|Torsion_Change_|Torsion_Final_")
  vnames <- str_remove_all(vnames, "VarVal_Change_|VarVal_Final_|FlxExt_Change_|FlxExt_Final_")
  vnames <- str_remove_all(vnames, "_Percentage|_Bone_Shortening")
  
  names(surg)[ix] <- vnames
  
  options(warn = -1)
  surg <- 
    surg %>% 
    mutate(across(which(ix), ~ as.numeric(.)))
  options(warn = 0)
  
  return(surg)
  
}