get_patient_info <- function(xpt, deid) {
  library(RODBC)
  
  MRN <- xpt$MRN[1]
  Event_Date <- xpt$Event_Date[1]
  Exam_ID <- xpt$Exam_ID[1]
  
  # Get Tables -----
  con <- odbcConnect("MotLabDB")
  tbllist <- sqlTables(con) |>  filter(TABLE_SCHEM == "dbo")
  
  Patient <- sqlFetch(con, "Patient") |> as_tibble()
  Patient_Events <- sqlFetch(con, "Patient_Events") |> as_tibble()
  .Patient_Event_Types <- sqlFetch(con, "_Patient_Event_Types") |> as_tibble()
  Exam <- sqlFetch(con, "Exam") |> as_tibble()
  Services <- sqlFetch(con, "Services") |> as_tibble()
  Problems <- sqlFetch(con, "Problems") |> as_tibble()
  .Problem <- sqlFetch(con, "_Problem") |> as_tibble()
  .Delivery <- sqlFetch(con, "_Delivery") |> as_tibble()
  SurgDefine <- sqlFetch(con, "SurgDefine") |> as_tibble() |> rename(ProcLoc_Desc2 = Procloc_Desc2)
  Surgical <- sqlFetch(con, "Surgical") |> as_tibble()
  Surgical_Details <- sqlFetch(con, "Surgical_Details") |> as_tibble()
  Treatment <- sqlFetch(con, "Treatment") |> as_tibble()
  .Side <- sqlFetch(con, "_Side") |> as_tibble()
  .Procedure <- sqlFetch(con, "_Procedure") |> as_tibble()
  .ProcType <- sqlFetch(con, "_ProcType") |> as_tibble()
  .ProcLoc <- sqlFetch(con, "_ProcLoc") |> as_tibble()
  
  close(con)
  
  # TODO Concerns -----
  concerndat <- 
    Exam |> 
    filter(Exam_ID == xpt$Exam_ID[1]) 
  
  # TODO Current Goals -----
  currgoaldat <- 
    Exam |> 
    filter(Exam_ID == xpt$Exam_ID[1])
  
  # Birth History -----
  birthdat <- 
    xpt |> 
    filter(SIDE == "L") |> 
    select(Delivery_Weeks, BIRTH_WEIGHT, NICU_Weeks, VENTILATOR, Ventilator_Weeks) |> 
    mutate(
      Delivery_Weeks = 42 + Delivery_Weeks,
      BIRTH_WEIGHT = round(BIRTH_WEIGHT * 2.20462, 1),
      birthpound = floor(BIRTH_WEIGHT),
      birthoz = round(16 * (BIRTH_WEIGHT - birthpound))
    )
  
  ix <- which(is.na(birthdat))
  birthdat[ix] <- "?"
  
  # Birth Problems -----
  probdat <- 
    Patient |> 
    mutate(
      MRN = floor(Hosp_Num/10),
      Event_Date = as.Date(Event_Date)
    ) |>
    as_tibble() |> 
    left_join(Patient_Events) |> 
    select(-Event_Time) |> 
    left_join(Exam) |> 
    select(-c(Therapist_ID, Technician_ID, SHCC_PO_NUM, DxCharges_ID)) |> 
    filter(
      MRN == params$MRN,
      Event_Date == params$Event_Date
    ) |> 
    select(Patient_ID, MRN, Event_Date, Patient_Event_ID, Exam_ID) |> 
    left_join(Problems) |> 
    left_join(.Problem) |> 
    mutate(Event_Date = as.Date(Event_Date)) |> 
    mutate(
      Problem_Type = case_when(
        Problem_Type == "A" ~ "Prenatal",
        Problem_Type == "B" ~ "Delivery",
        Problem_Type == "C" ~ "Neonatal"
      )
    ) |> 
    mutate(
      Problem = str_remove_all(Problem, "^[A-Z]-"),
      Problem = str_to_sentence(Problem)
    ) |> 
    select(Problem_Type, Problem) |> 
    group_by(Problem_Type)
  
  # Check for no problems
  if(sum(!is.na(probdat$Problem)) == 0){
    probdat <- tibble(Problem_Type = "No Problems Listed", Problem = "")
  }
  
  birthtit <- glue("Gestation = {birthdat$Delivery_Weeks} wks, Weight = {birthdat$birthpound}lb {birthdat$birthoz}oz, NICU = {birthdat$NICU_Weeks} wks, Ventilator = {birthdat$Ventilator_Weeks} wks")
  
  tbirth <- 
    probdat |> 
    gt() |> 
    sub_missing(
      columns = everything(),
      missing_text = ""
    ) |>
    tab_header(
      title = "Birth History",
      subtitle = birthtit
    ) |> 
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_title()
    ) |> 
    tab_style(
      style = cell_text(indent = px(40)),
      locations = cells_body()
    ) |> 
    tab_style(
      style = cell_text(weight = 500),
      locations = cells_row_groups()
    ) |> 
    tab_options(
      column_labels.hidden = TRUE
    )
  
  # Developmental History -----
  # NOTICED_PROB seems to vary in units (mo vs. yr)
  devdat <- 
    xpt |> 
    filter(SIDE == "L") |> 
    select(NOTICED_PROB, DEV_FIRST_STEP, INIT_AMB_DEV) |> 
    mutate(
      NOTICED_PROB = round(12 * NOTICED_PROB)
    )
  
  ix <- which(is.na(devdat))
  devdat[ix] <- "?"
  
  tdev <- 
    devdat |> 
    gt() |> 
    tab_header("Developmental History") |> 
    cols_label(
      NOTICED_PROB ~ "Noticed Problem [mo]",
      DEV_FIRST_STEP ~ "First Step [yr]",
      INIT_AMB_DEV ~ "Initial Walking Aid"
    ) |> 
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_title()
    ) |> 
    tab_style(
      style = cell_text(weight = 500),
      locations = cells_column_labels()
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(cells_column_labels(), cells_body())
    ) |> 
    cols_width(
      everything() ~ px(200)
    )
  
  
  # Treatment history -----
  surgdat <- 
    Patient |> 
    mutate(MRN = floor(Hosp_Num/10)) |> 
    filter(MRN == xpt$MRN[1]) |> 
    left_join(Patient_Events) |> 
    left_join(Treatment) |> 
    left_join(Surgical) |> 
    select(-Surgical_ID) |> 
    
    left_join(.Procedure) |> 
    left_join(.ProcLoc, join_by(ProcLoc_ID1 == ProcLoc_ID)) |> 
    rename(ProcLoc_name1 = ProcLoc) |> 
    left_join(.ProcLoc, join_by(ProcLoc_ID2 == ProcLoc_ID)) |> 
    rename(ProcLoc_name2 = ProcLoc) |> 
    
    left_join(.ProcType, join_by(ProcType_ID1 == ProcType_ID)) |>
    rename(ProcType_name1 = ProcType) |> 
    left_join(.ProcType, join_by(ProcType_ID2 == ProcType_ID)) |>
    rename(ProcType_name2 = ProcType) |> 
    select(-IsUnknown) |> 
    left_join(.Side) |> 
    select(-IsUnknown) |> 
    
    left_join(SurgDefine) |>
    select(Birth_Date, Event_Date, SurgCode, Side, Treatment_ID, Patient_ID, 
           Patient_Event_ID, matches("_name|_Desc")) |> 
    mutate(
      Age = as.numeric(difftime(Event_Date, Birth_Date, units = "days"))/365.25, 
      Ageyr = floor(Age),
      Agemo = round((Age - Ageyr) * 12),
      Agestr = glue("{Ageyr} yr + {Agemo} mo")
    ) |> 
    mutate(
      across(
        matches("name|Desc"),
        ~ ifelse(. == "Unknown", "", .)
      )
    ) |>
    mutate(
      across(
        starts_with("Proc"),
        ~ ifelse(is.na(.), "", .)
      ),
      SurgCode = ifelse(is.na(SurgCode), "NO META", SurgCode)
    ) |> 
    mutate(
      Side = factor(Side, levels = c("Left", "Right"))
    ) %>% 
    drop_na(Side) |> 
    group_by(Event_Date) |> 
    arrange(desc(Event_Date), Side) |> 
    filter(Event_Date <= params$Event_Date) |> 
    mutate(
      Event_Date = 
        ifelse(
          deid, 
          glue("{year(Event_Date)}, Age: {Ageyr} yr"),
          glue("{Event_Date}, Age: {Agestr}")
        )
    )|> 
    select(-c(Birth_Date, Age, Ageyr, Agemo, Agestr, Proc_Desc, matches("Desc1|Desc2|Loc1|Loc2"))) |> 
    arrange(Event_Date)
  
  # Lookup surgery names
  temp <- vlabs[surgdat$SurgCode]
  surgdat$SurgCode <- ifelse(is.na(temp), surgdat$SurgCode, temp)

  
  tsurg <- NULL
  if(nrow(surgdat) > 0){
    tsurg <- 
      surgdat |> 
      gt() |> 
      tab_header("Treatment History") |> 
      cols_hide(
        ends_with("_ID")
      ) |> 
      cols_label(
        SurgCode = "Treatment",
        Procedure_Desc = "Description",
        ProcType_name1 = "Type",
        ProcLoc_name1 = "Location",
      ) |> 
      tab_style(
        style = cell_text(indent = px(30)),
        locations = cells_body(columns  = SurgCode)
      ) |> 
      tab_style(
        style = cell_text(weight = "bold", align = "left"),
        locations = cells_title()
      ) |> 
      tab_style(
        style = cell_text(weight = 500, align = "left"),
        locations = cells_column_labels()
      ) |> 
      tab_style(
        style = cell_text(style = "italic", align = "left", v_align = "bottom"),
        locations = cells_row_groups()
      ) |> 
      tab_style(
        style = cell_text(size = px(13)),
        locations = cells_body()
      ) |> 
      data_color(
        columns = Side,
        target_columns = everything(),
        method = "factor",
        levels = c("Left", "Right"),
        palette = qualitative_hcl(2, palette = "Dark 3", , c1 = 55, l1 = 55)
      ) |>
      cols_merge(
        columns = c(ProcType_name1, ProcType_name2)
      )  |> 
      cols_merge(
        columns = c(ProcLoc_name1, ProcLoc_name2)
      )|> 
      cols_width(
        SurgCode ~ px(250),
        Side ~ px(50),
        Procedure_Desc ~ px(140),
        ProcType_name1 ~ px(130),
        ProcLoc_name1 ~ px(130)
      ) |> 
      tab_options(
        row_group.padding = px(8),
        data_row.padding.horizontal = px(10)
      )
  } else{
    tsurg <- NULL
  }
  
  return(list("tbirth" = tbirth, "tdev" = tdev, "tsurg" = tsurg))
}
