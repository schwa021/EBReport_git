add_GOAL <- function(FD, items_lookup.tbl, rating_lookup.tbl, GOAL_items.tbl, GOAL.tbl){
  
  items_lookup.tbl <- items_lookup.tbl %>% 
    rename(Item_Abbreviation = Abbreviation)
  
  rating_lookup.tbl <- rating_lookup.tbl %>% 
    rename(Rating_Description = Description,
           Rating_Abbreviation = Abbreviation)
  
  
  # Get item and rating names
  GOAL_items <- inner_join(GOAL_items.tbl, items_lookup.tbl)
  GOAL_items <- inner_join(GOAL_items, rating_lookup.tbl)
  
  
  # Join items with domain scores
  GOAL_all <- inner_join(GOAL.tbl, GOAL_items)
  
  
  # Rename
  GOALdat <- as_tibble(GOAL_all) %>% 
    rename(
      ADL_Indep = A_Score,
      Gait_Func_Mobility = B_Score,
      Pain_Discomfort_Fatigue = C_Score,
      Activities_Sports_Rec = D_Score,
      Gait_Pattern_Appearance = E_Score,
      Braces_Mobility = F_Score,
      Image_Esteem = G_Score
    ) %>% 
    select(-c(GOAL_Type_ID, GOAL_Rater_ID, GOAL_Rater_Comments, Comments, GOAL_Item_ID, Item_ID, Qualifier_Val, OptOut, Comment, GOAL_Items_ID, Item_Abbreviation, Rating_Abbreviation, Rating_Description, Section_ID, Goal_Val, GOAL_Rating_ID))
  
  
  # Make wide
  GOAL <-
    pivot_wider(GOALdat,
                names_from = c(Section, Item),
                values_from = (Rating_Val))
  
  
  GOAL <- GOAL %>% 
    select(sort(names(GOAL))) %>% 
    relocate(TOTAL_Score, Activities_Sports_Rec, ADL_Indep, Braces_Mobility, Gait_Func_Mobility, Gait_Pattern_Appearance, Image_Esteem, Pain_Discomfort_Fatigue)
  
  vlist <- str_subset(names(GOAL), "^A_|^B_|^C_|^D_|^E_|^G_")
  vlist <- str_subset(vlist, "Free Text", negate = T)
  
  GOAL <- GOAL %>% 
    mutate(across(all_of(vlist), ~ factor(.))) %>% 
    mutate(across(all_of(vlist), ~ fct_na_value_to_level(., level = "Missing")))
  
  
  FD <- FD %>% 
    left_join(GOAL)
  
  return(FD)
}