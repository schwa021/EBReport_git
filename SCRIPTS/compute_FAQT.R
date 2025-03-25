compute_FAQT <- function(x){
  
  wtrasch22 <- 
    c("ICE_ROLL_SKATE"=2.71, "JUMP_ROPE"=2.30, "RIDE_2_WHEEL"=2.02, "HOP_RIGHT"=1.66,
      "HOP_LEFT"=1.65, "WALK_STAIR_WO_RAIL"=0.92, "RUN_W_CONTROL"=0.84, 
      "ESCAL_INDEP"=0.72,  "JUMP_OFF_STEP"=0.43,  "BUS_ON_OFF"=0.25, 
      "WALK_W_FRAG_OBJ"=0.24, "RUN"=0.19, "KICK_LEFT"=-0.19, "RIDE_3_WHEEL"=-0.19,
      "KICK_RIGHT"=-0.25, "STEP_OVER_LEAD_LT"=-0.29, "STEP_OVER_LEAD_RT"=-0.30,
      "STEP_BACK"=-0.39, "STEP_CURB"=-0.41, "TURN_TIGHT_AREA"=-0.42,
      "WALK_W_OBJ"=-0.82, "WALK_STAIR_W_RAIL"=-0.90
    )
  
  wtrasch10 <- c(
    "10-Walks, runs, climbs without difficulty or assistance" = 2.76,
    "09-Walks community distances; OK with curbs, uneven terrain; needs help on stairs, climbing" = 1.18,
    "08-Walks community distances; needs supervision for safety on curbs or uneven terrain" = 0.32,
    "07-Walks community distances; needs help for curbs or uneven terrain" = -.37,
    "06-Walks 15-50 feet outside the home; usually uses wheelchair or stroller" = -.97,
    "05-Household distances (home/classroom); indoor walking only" = -1.47,
    "04-Household distances, slowly; does not walk for preferred mobility"  = -Inf,
    "03-Therapy exercise and/or less than typical household distances"  = -Inf,
    "02-Some steps with help; cannot take full weight"  = -Inf,
    "01-Cannot take any steps"  = -Inf
  )
  
# Get scores for 22 skills -----
  scores22 <- 
    x %>% 
    select(all_of(names(wtrasch22))) %>% 
    mutate(
      across(
        everything(),
        ~ case_when(
          . == "Easy" ~ plogis(wtrasch22[[cur_column()]]),
          . == "A little hard" ~ plogis(wtrasch22[[cur_column()]]),
          . == "Very hard" ~ 0,
          . == "Can't do at all" ~ 0,
          . == "Too young" ~ 0,
          . == "Missing" ~ NA_real_,
          is.na(.) ~ NA_real_,
          TRUE ~ NA_real_
        )
      )
    )

  # Get scores for overall walking -----
  scores10 <-
    x %>% 
    select(FAQ) %>% 
    mutate(FAQ = plogis(wtrasch10[as.character(FAQ)]))
  
  # Limits for all skills (sumall) and no skills (sumnone)
  sumall <- as.numeric(sum(plogis(wtrasch22)) + plogis(wtrasch10[1]))
  sumnone <- 0
  
  # Compute sum of scores
  sumpt <- rowSums(scores22) + scores10

  # Scale to 0-100
  res <- round(100 * (sumpt - sumnone) / (sumall - sumnone), 1)
  x$FAQT <- res %>% pull(FAQ)
  
  return(x)
}
