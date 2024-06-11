read_and_organize <- function(dd) {
  # Check if this is pre-post or pre-only -----
  pp <- !is.null(dd$agePost)
  
  # If this is pre-post data, use the following code -----
  if (pp) {
    ddret <-
      dd %>%
      ## General filter
      filter(                                              
        agePost <= 18,
        agePost - age < 3,
        !is.na(GDI),
        Sex != "Missing",
        Event_Date >= as.Date("1994-01-31")
      ) %>%
      ## Affected as category
      mutate(affected = factor(affected)) %>% 
      ## Define eras
      mutate(era = cut(
        as.numeric(year(Event_Date)),
        c(0, 2005, 2015, 10E6),
        labels = c("<2005", "2005-2015", ">2015")
      )) %>%
      ## lump small dx/mod categories, define year
      mutate(
        dxmod = fct_lump_min(dxmod, 10),
        Year = year(Event_Date)
      ) %>%
      ## Recode rare diagnoses --> Other
      mutate(
        dx = fct_recode(
          dx,
          `Developmental variants` = "Hip disorder",
          `Developmental variants` = "Knee disorder"
        ),
        dx = fct_lump_min(min = 50, dx)
      ) %>%
      ## Choose EOS or PE for Anteversion and Tibial Torsion
      mutate(FemTor = ifelse(!is.na(EOS_FemTor), EOS_FemTor, ANTEVERSION)) %>% 
      ## Recode FDO_DFEO --> FDO and DFEO
      mutate(
        prior_Femoral_Derotation_Osteotomy = prior_Femoral_Derotation_Osteotomy + prior_FDO_DFEO,
        prior_DFEO = prior_DFEO + prior_FDO_DFEO,
        prior_Femoral_Derotation_OsteotomyPost = prior_Femoral_Derotation_OsteotomyPost +
          prior_FDO_DFEOPost,
        prior_DFEOPost = prior_DFEOPost + prior_FDO_DFEOPost,
        interval_Femoral_Derotation_Osteotomy = interval_Femoral_Derotation_Osteotomy +
          interval_FDO_DFEO,
        interval_DFEO = interval_DFEO + interval_FDO_DFEO
      ) %>%
      select(-matches("FDO_DFEO")) %>%
      ## Define DFEO_Patellar_Advance
      mutate(
        prior_DFEO_Patellar_Advance = prior_DFEO > 0 & prior_Patellar_Advance > 0,
        prior_DFEO = prior_DFEO > 0 & prior_DFEO_Patellar_Advance == 0,
        prior_Patellar_Advance = prior_Patellar_Advance > 0 & prior_DFEO_Patellar_Advance == 0,
        interval_DFEO_Patellar_Advance = interval_DFEO > 0 & interval_Patellar_Advance > 0,
        interval_DFEO = interval_DFEO > 0 & interval_DFEO_Patellar_Advance == 0,
        interval_Patellar_Advance = interval_Patellar_Advance > 0 & interval_DFEO_Patellar_Advance == 0,
      ) %>% 
      ## Recode surgery as 1 or 0
      mutate(across(
        c(starts_with("interval"), starts_with("prior")),
        ~ factor(
          . > 0,
          levels = c("TRUE", "FALSE"),
          labels = c(1, 0)
        )
      )) %>%
      drop_na(starts_with("Feat_Ang")) %>%
      as_tibble()
  }
  
  # If this is pre-only data use the following code
  if (!pp) {
    # Organize data
    ddret <-
      dd %>%
      ## General filter
      filter(                                              
        # age <= 18,
        # !is.na(GDI),
        Sex != "Missing",
        Event_Date >= as.Date("1994-01-31")
      ) %>%
      ## Define eras
      mutate(era = cut(
        as.numeric(year(Event_Date)),
        c(0, 2005, 2015, 10E6),
        labels = c("<2005", "2005-2015", ">2015")
      )) %>%
      ## Affected as category
      mutate(affected = factor(affected)) %>% 
      ## lump small dx/mod categories
      mutate(
        dx = fct_recode(
          dx,
          `Developmental variants` = "Hip disorder",
          `Developmental variants` = "Knee disorder"
        )
      ) %>%
      ## Choose EOS or PE for Anteversion and Tibial Torsion
      mutate(FemTor = ifelse(!is.na(EOS_FemTor), EOS_FemTor, ANTEVERSION)) %>% 
      # Define year
      mutate(Year = year(Event_Date)) %>%
      ## Recode FDO_DFEO --> FDO and DFEO
      mutate(
        prior_Femoral_Derotation_Osteotomy = prior_Femoral_Derotation_Osteotomy + prior_FDO_DFEO,
        prior_DFEO = prior_DFEO + prior_FDO_DFEO,
      ) %>%
      select(-matches("FDO_DFEO")) %>%
      ## Define DFEO_Patellar_Advance
      mutate(
        prior_DFEO_Patellar_Advance = prior_DFEO > 0 & prior_Patellar_Advance > 0,
        prior_DFEO = prior_DFEO > 0 & prior_DFEO_Patellar_Advance == 0,
        prior_Patellar_Advance = prior_Patellar_Advance > 0 & prior_DFEO_Patellar_Advance == 0,
      ) %>% 
      ## Recode surgery as 1 or 0
      mutate(across(
        c(starts_with("interval"), starts_with("prior")),
        ~ factor(
          . > 0,
          levels = c("TRUE", "FALSE"),
          labels = c(1, 0)
        )
      )) %>%
      # drop_na(starts_with("Feat_Ang")) %>%
      as_tibble()
  }
  
  return(ddret)
  
}