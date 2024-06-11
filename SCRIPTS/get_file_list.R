get_file_list <- function(){
  con <- dbConnect(odbc::odbc(), dsn = "MotLabDB")
  tbllist <- dbListTables(con)
  Side <- tbl(con, "_Side")
  Diagnosis <- tbl(con, "Diagnosis") %>% filter(Diag_Level == 1)
  Diagnosis_lookup <- tbl(con, "_DxMain")
  Patient <- tbl(con, "Patient")
  Patient_Events <- tbl(con, "Patient_Events")
  Exam <- tbl(con, "Exam")
  Services <- tbl(con, "Services")
  PE <- tbl(con, "Physical_Exam")
  TMH <- tbl(con, "Trials_Motion_HDR")
  Trial_Type <- tbl(con, "Trial_Type")
  .Trial_Type <- tbl(con, "_Trial_Type")
  .Device <- tbl(con, "_Device")
  .DeviceType <- tbl(con, "_DeviceType")
  
  # PElim <- PE %>% 
  #   select(Physical_Exam_ID, Service_ID, WEIGHT, LEG_LENGTH_L, LEG_LENGTH_R) %>% 
  #   as_tibble()
  
  xlist <- 
    Patient %>% 
    left_join(Diagnosis) %>% 
    inner_join(Diagnosis_lookup) %>% 
    inner_join(Patient_Events) %>% 
    inner_join(Exam) %>% 
    inner_join(Services) %>% 
    inner_join(TMH) %>% 
    inner_join(Trial_Type) %>% 
    filter(TrialType_ID == 17 | TrialType_ID == 18) %>%
    filter(Hosp_Num < 9999990) %>% 
    mutate(year = year(Event_Date)) %>% 
    filter(year > 1993) %>% 
    
    left_join(.Device, by = c("OrthDev_L_Device_ID" = "Device_ID")) |> 
    rename(OrthDev_L = Device) |> 
    left_join(.Device, by = c("OrthDev_R_Device_ID" = "Device_ID")) |> 
    rename(OrthDev_R = Device) |> 
    
    # Get orthotic and assistive devices
    left_join(.Device, by = c("AssistDev_L_Device_ID" = "Device_ID")) |> 
    rename(AssistDev_L = Device) |> 
    left_join(.Device, by = c("AssistDev_R_Device_ID" = "Device_ID")) |> 
    rename(AssistDev_R = Device) |> 
    select(!matches("^Assist.*ID$|^Orth.*ID$")) |>

    as_tibble() |> 
    filter(str_detect(Filename, "(?i)c3d|gcd"))
  
  pelist <- 
    Patient %>% 
    inner_join(Patient_Events) %>% 
    inner_join(Exam) %>% 
    inner_join(Services) %>% 
    inner_join(PE) %>% 
    select(Exam_ID, WEIGHT, LEG_LENGTH_L, LEG_LENGTH_R) %>% 
    as_tibble()
  
  xlist <- 
    xlist %>% 
    inner_join(pelist)%>% 
    distinct() %>% 
    mutate(across(c(WEIGHT, LEG_LENGTH_L, LEG_LENGTH_R), ~ ifelse(. > 0, ., NA))) |> 
    mutate(MRN = trunc(Hosp_Num/10))
  
  dbDisconnect(con)
  
  return(xlist)
}