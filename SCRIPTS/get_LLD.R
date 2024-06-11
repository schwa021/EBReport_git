# Compute LLD (L - R)
get_LLD <- function(x, v, vd){
  
  vf <- function(q){
    y <- mean(q, na.rm = T)
    return(y)
  }
  
  temp <- 
    x |> 
    group_by(Exam_ID) |> 
    select(Exam_ID, all_of(v), SIDE) |> 
    pivot_wider(names_from = SIDE, values_from = v, values_fn = vf) |> 
    mutate({{vd}} := L - R) |> 
    pivot_longer(c(L, R), names_to = "SIDE") |> 
    mutate({{vd}} := ifelse(SIDE == "L", .data[[vd]], -.data[[vd]])) |> 
    select(Exam_ID, all_of(vd), SIDE)
  
  x <- left_join(x, temp)  
  
  return(x)
}