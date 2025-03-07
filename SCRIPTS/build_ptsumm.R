build_ptsumm <- function(params, xpt){
  
  if(!params$deid){
    ptname <- glue("{xpt$Child_First} {xpt$Child_Last}")[1]
    ptdate <- format(as.Date(params$Event_Date), "%b %d, %Y")
    ptmrn <- xpt$MRN[1]
  } else {
    ptname <- ifelse(xpt$Sex[1] == F, "Trillian Astra", "Arthur Dent")
    ptdate <- format(as.Date(params$Event_Date), "%b %Y")
    ptmrn <- xpt$Exam_ID[1]
  }
  
  if(!params$deid){
    tit <- glue("{ptname} ({ptmrn}), Exam Date: {ptdate}")
  } else {
    tit <- glue("{ptname} ({ptmrn}), Exam Date: {ptdate}")
  }
  
  # Build patient summary data -----
  tdat <- 
    xpt %>%  
    filter(SIDE == "L") |> 
    select(age, dx, dxmod, dxside, GMFCS_meas, FAQ) |> 
    mutate(age = round(age,1)) |> 
    rename_with(
      .fn = ~ vlabs[.],
      .cols = everything()
    ) %>% 
    rename("GMFCS" = "GMFCS (measured)")
  
  ptsumm <- 
    tdat |> 
    gt() |> 
    tab_header(tit) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = list(cells_body(), cells_column_labels())
    ) |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |> 
    cols_width(
      c(Age, Diagnosis, `Diagnosis Modifier`, GMFCS) ~ px(100),
      `Functional Assessment Questionnaire` ~ 200
    )
  
  return(ptsumm)
}