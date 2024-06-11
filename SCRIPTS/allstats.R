# Get Statistics ---------------------------------------------------------------
allstats <- function(x){
  
  if(nrow(x) == 0){
    return(NULL)
  }
  
  # Value at initial contact ----
  ic <- 
    x[1,] %>% 
    pivot_longer(cols = everything(), names_to = c("side", "Exam_ID", "name"), names_sep ="_") %>% 
    mutate(stat = "ic")
  
  # Value at opposite foot off ----
  ofo <- 
    x[50,] %>% 
    pivot_longer(cols = everything(), names_to = c("side", "Exam_ID", "name"), names_sep ="_") %>% 
    mutate(stat = "ofo")
  
  # Value at opposite foot contact ----
  ofc <- 
    x[100,] %>% 
    pivot_longer(cols = everything(), names_to = c("side", "Exam_ID", "name"), names_sep ="_") %>% 
    mutate(stat = "ofc")
  
  # Value at foot off ----
  fo <- 
    x[150,] %>% 
    pivot_longer(cols = everything(), names_to = c("side", "Exam_ID", "name"), names_sep ="_") %>% 
    mutate(stat = "fo")
  
  # Value at mid stance ----
  midsta <- 
    x[75,] %>% 
    pivot_longer(cols = everything(), names_to = c("side", "Exam_ID", "name"), names_sep ="_") %>% 
    mutate(stat = "midsta")
  
  # Value at mid swing ----
  midswi <- 
    x[175,] %>% 
    pivot_longer(cols = everything(), names_to = c("side", "Exam_ID", "name"), names_sep ="_") %>% 
    mutate(stat = "midswi") 
  
  # Stance ----
  meansta <- getstat(x, 1:150, "mean", "meansta")
  minsta <- getstat(x, 1:150, "min", "minsta")
  tminsta <- getstat(x, 1:150, "which.min", "t_minsta")
  maxsta <- getstat(x, 1:150, "max", "maxsta")
  tmaxsta <- getstat(x, 1:150, "which.max", "t_maxsta")
  
  # Swing ----
  meanswi <- getstat(x, 150:200, "mean", "meanswi")
  minswi <- getstat(x, 150:200, "min", "minswi")
  tminswi <- getstat(x, 150:200, "which.min", "t_minswi")
  maxswi <- getstat(x, 150:200, "max", "maxswi")
  tmaxswi <- getstat(x, 150:200, "which.max", "t_maxswi")
  
  y <-  bind_rows(
    ic, ofo, midsta, ofc, fo, midswi,
    meansta, minsta, tminsta, maxsta, tmaxsta,
    meanswi, minswi, tminswi, maxswi, tmaxswi
  )
  
  return(y)
  
}