build_goalimportant <- function(xpt, datpre){
  # Individual question scores -----
  scores_ref <-
    datpre |>
    select(GMFCS, matches("^[ABCDEFG]_", perl = TRUE)) |>
    select(!matches("Post$"))
  
  # Individual question scores -----
  scores_pt <-
    xpt |>
    filter(SIDE == "L")
    select(GMFCS, matches("^[ABCDEFG]_", perl = TRUE)) |>
    select(!matches("Post$"))
    
}
  