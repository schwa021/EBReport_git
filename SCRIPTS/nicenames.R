# Function to take raw names from c3d file and turn them into nice names with
# proper factor ordering

nicenames <- function(name, typestr){
  
  # Angle ----------------------------------------------------------------------
  if(typestr == "Angle"){
    namex = case_when(
      str_detect(name, "^Pel\\w*X$") ~ "Pel.Ang.Sag",
      str_detect(name, "^Pel\\w*Y$") ~ "Pel.Ang.Cor",
      str_detect(name, "^Pel\\w*Z$") ~ "Pel.Ang.Trn",
      
      str_detect(name, "^Hip\\w*X$") ~ "Hip.Ang.Sag",
      str_detect(name, "^Hip\\w*Y$") ~ "Hip.Ang.Cor",
      str_detect(name, "^Hip\\w*Z$") ~ "Hip.Ang.Trn",
      
      str_detect(name, "^Knee\\w*X$") ~ "Kne.Ang.Sag",
      str_detect(name, "^Knee\\w*Y$") ~ "Kne.Ang.Cor",
      str_detect(name, "^Knee\\w*Z$") ~ "Kne.Ang.Trn",
      
      str_detect(name, "^Ankle\\w*X$") ~ "Ank.Ang.Sag",
      str_detect(name, "^Ankle\\w*Y$") ~ "Ank.Ang.Cor",
      str_detect(name, "^Ankle\\w*Z$") ~ "Ank.Ang.Trn",
      
      str_detect(name, "^Foot\\w*X$") ~ "Foo.Ang.Sag",
      str_detect(name, "^Foot\\w*Y$") ~ "Foo.Ang.Cor",
      str_detect(name, "^Foot\\w*Z$") ~ "Foo.Ang.Trn",
      
      TRUE ~ name
    )
    
    namex <- factor(namex, 
                    levels = c("Pel.Ang.Cor", "Pel.Ang.Sag", "Pel.Ang.Trn",
                               "Hip.Ang.Cor", "Hip.Ang.Sag", "Hip.Ang.Trn",
                               "Kne.Ang.Cor", "Kne.Ang.Sag", "Kne.Ang.Trn",
                               "Ank.Ang.Cor", "Ank.Ang.Sag", "Ank.Ang.Trn",
                               "Foo.Ang.Cor", "Foo.Ang.Sag", "Foo.Ang.Trn"))
  }
  
  
  # Moment ---------------------------------------------------------------------
  if(typestr == "Moment"){
    namex = case_when(
      str_detect(name, "^Hip\\w*Y$") ~ "Hip.Mom.Sag",
      str_detect(name, "^Hip\\w*X$") ~ "Hip.Mom.Cor",
      str_detect(name, "^Hip\\w*Z$") ~ "Hip.Mom.Trn",
      
      str_detect(name, "^Knee\\w*Y$") ~ "Kne.Mom.Sag",
      str_detect(name, "^Knee\\w*X$") ~ "Kne.Mom.Cor",
      str_detect(name, "^Knee\\w*Z$") ~ "Kne.Mom.Trn",
      
      str_detect(name, "^Ankle\\w*Y$") ~ "Ank.Mom.Sag",
      str_detect(name, "^Ankle\\w*X$") ~ "Ank.Mom.Cor",
      str_detect(name, "^Ankle\\w*Z$") ~ "Ank.Mom.Trn",
      
      TRUE ~ name
    )
    
    namex <- factor(namex, 
                    levels = c("Hip.Mom.Cor", "Hip.Mom.Sag", "Hip.Mom.Trn",
                               "Kne.Mom.Cor", "Kne.Mom.Sag", "Kne.Mom.Trn",
                               "Ank.Mom.Cor", "Ank.Mom.Sag", "Ank.Mom.Trn"))
  }
  
  
  # Power ----------------------------------------------------------------------
  if(typestr == "Power"){
    namex = case_when(
      str_detect(name, "^Hip\\w*Z$") ~ "Hip.Pwr",
      str_detect(name, "^Knee\\w*Z$") ~ "Kne.Pwr",
      str_detect(name, "^Ankle\\w*Z$") ~ "Ank.Pwr",
      
      TRUE ~ name
    )
    
    namex <- factor(namex, 
                    levels = c("Hip.Pwr", "Kne.Pwr", "Ank.Pwr"))
    
  }
  
  
  # Trunk ----------------------------------------------------------------------
  if(typestr == "Trunk"){
    namex = case_when(
      str_detect(name, "X$") ~ "Trk.Ang.Sag",
      str_detect(name, "Y$") ~ "Trk.Ang.Cor",
      str_detect(name, "Z$") ~ "Trk.Ang.Trn",
      
      TRUE ~ name
    )
    
    namex <- factor(namex, 
                    levels = c("Trk.Ang.Cor", "Trk.Ang.Sag", "Trk.Ang.Trn"))
  }  
  
  
  # Reaction -------------------------------------------------------------------
  if(typestr == "Reaction"){
    namex = case_when(
      str_detect(name, "^G\\w*Force_X$") ~ "GRF.AntPst",
      str_detect(name, "^G\\w*Force_Y$") ~ "GRF.MedLat",
      str_detect(name, "^G\\w*Force_Z$") ~ "GRF.UpDwn",
      str_detect(name, "^G\\w*Moment_X$") ~ "GRM.AntPst",
      str_detect(name, "^G\\w*Moment_Y$") ~ "GRM.MedLat",
      str_detect(name, "^G\\w*Moment_Z$") ~ "GRM.UpDwn",
      
      TRUE ~ name
    )
    
    namex <- factor(namex, 
                    levels = c("GRF.AntPst", "GRF.MedLat", "GRF.UpDwn",
                               "GRM.AntPst", "GRM.MedLat", "GRM.UpDwn"))
  }  

  
  # Length ---------------------------------------------------------------------
  if(typestr == "Length"){
    namex = case_when(
      str_detect(name, "add_long") ~ "Add.Long",
      str_detect(name, "semimem") ~ "Semimem",
      str_detect(name, "psoas") ~ "Psoas",
      str_detect(name, "rect_fem") ~ "Rect.Fem",
      str_detect(name, "med_gas") ~ "Med.Gas",
      
      TRUE ~ name
    )
    
    namex <- factor(namex, 
                    levels = c("Psoas", "Add.Long", "Semimem", "Rect.Fem", "Med.Gas"))
  }  
  return(namex)
}
