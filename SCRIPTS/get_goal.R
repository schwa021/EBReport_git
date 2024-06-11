get_goal <- function(params) {
  library(RODBC)
  
  # Open connection -----
  con <- odbcConnect("MotLabDB")
  tbllist <- sqlTables(con) %>% filter(TABLE_SCHEM == "dbo")
  
  
  # Get tables -----
  Patient.tbl <- sqlFetch(con, "Patient")
  Patient_Events.tbl <- sqlFetch(con, "Patient_Events")
  Exam.tbl <- sqlFetch(con, "Exam")
  GOAL_Main <- sqlFetch(con, "GOAL_Main") |> as_tibble()
  GOAL_Items <- sqlFetch(con, "GOAL_Items") |> as_tibble()
  .GOAL_Goal <- sqlFetch(con, "_GOAL_Goal") |> as_tibble()
  .GOAL_Item <- sqlFetch(con, "_GOAL_Item") |> as_tibble()
  .GOAL_Qualifier <- sqlFetch(con, "_GOAL_Qualifier") |> as_tibble()
  .GOAL_Rating <- sqlFetch(con, "_GOAL_Rating") |> as_tibble()
  .GOAL_Type <- sqlFetch(con, "_GOAL_Type") |> as_tibble()
  
  close(con)
  
  
  # Build GOAL table -----
  goal <-
    Patient.tbl %>%
    select(sort(names(Patient.tbl))) |>
    as_tibble() %>%
    left_join(Patient_Events.tbl) %>%
    filter(Patient_Event_Type_ID == 2) |>
    mutate(MRN = floor(Hosp_Num / 10)) |>
    filter(MRN == params$MRN) |>
    left_join(Exam.tbl) |>
    left_join(GOAL_Main) |>
    left_join(GOAL_Items) |>
    left_join(.GOAL_Rating) |>
    rename(Difficulty = Description) |>
    select(-Abbreviation) |>
    left_join(.GOAL_Item) |>
    select(-Abbreviation) |>
    left_join(.GOAL_Goal) |>
    select(-Abbreviation) |>
    drop_na(GOAL_Main_ID) |> 
    rename(Importance = Description)
  
  # Add comment goals -----
  ix <- str_length(goal$Comment) > 0
  goal$Item[ix] <- goal$Comment[ix]
  
  goal <-
    goal |>
    # mutate(Item = glue("{Section}-{Item}")) |> 
    select(MRN, Exam_ID, Event_Date, GOAL_Main_ID, Section, matches("^[A-G]_"), TOTAL_Score,
           Difficulty, Item, Importance) |> 
    mutate(
      Importance = factor(Importance, levels = c("Very important", "Somewhat important", "Not a goal"))
    )
  
  return(goal)
  
}
