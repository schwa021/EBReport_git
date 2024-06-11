build_goaltbl <- function(xpt, datpre) {
  
  # Domain scores -----
  getdomain <- function(dd) {
    y <-
      dd |>
      filter(SIDE == "L") |>
      select(
        GMFCS,
        matches(
          "^TOTAL_|^Activities_|^ADL_|^Braces_|^Gait_Func_|^Gait_Pattern_|^Image_|^Pain_Dis"
        )
      ) |>
      select(!matches("Post$")) |>
      pivot_longer(-GMFCS)
    
    return(y)
  }
  
  goaltot_ref <- getdomain(datpre)
  goaltot_pt <- getdomain(xpt)
  
  if(all(is.na(goaltot_pt$value))) return(NULL)
  
  getgoalsev <- function(xpt, datpre, v) {
    # Patient value -----
    gpt <- xpt[[v]][1]
    gmfcspt <- xpt[["GMFCS_meas"]][1]
    
    # Reference quantiles -----
    gref <- datpre |> filter(GMFCS == gmfcspt) |> pull(v)
    q <-
      quantile(gref,
               probs = c(.30, .60, .90),
               na.rm = TRUE)
    breaks <- c(-Inf, q, Inf)
    
    qall <- quantile(
      gref,
      probs = seq(0, 1, .01),
      na.rm = TRUE
    )
    
    qpt <- names(qall)[which.min(abs(qall - gpt))]
    qpt <- ifelse(length(qpt)==0, NA, qpt)
    
    # Compute severity -----
    if(is.na(gpt)){
      sev <- NA
    } else {
      sev <-  cut(gpt, breaks, labels = c(1, 2, 3, 4)) 
    }
    
    return(list("sev"=sev, "qtl"=qpt))
  }
  
  vv <- str_subset(
    names(datpre),
    "^TOTAL_|^Activities_|^ADL_|^Braces_|^Gait_Func_|^Gait_Pattern_|^Image_|^Pain_Dis"
  )
  
  tempsev <- vv |> map(\(v) getgoalsev(xpt, datpre, v)) |> unlist()
  ptsev <- tempsev[names(tempsev) == "sev"]
  ptqtl <- tempsev[names(tempsev) == "qtl"]
  
  
  domain <- fct_inorder(c("sev", "mod", "mil", "none"))
  # Choose palette -----
  clr <-
    sequential_hcl(
      n = 4,
      h = 360,
      c = c(85, NA, NA),
      l = c(55, 90),
      power = 1.8
    )
  pal <- clr
  
  tdat <-
    xpt |>
    filter(SIDE == "L") |>
    select(all_of(vv)) |>
    pivot_longer(everything()) |>
    mutate(
      sev = ptsev,
      qtl = ptqtl,
      name = str_replace_all(name, "_", " ")
    )
  
  t <-
    tdat |>
    gt() |>
    sub_missing(missing_text = "") |> 
    tab_header("GOAL Domain Scores") |>
    cols_hide(sev) |>
    cols_label(name ~ "Domain",
               value ~ "Score",
               qtl ~ "Percentile") |>
    fmt_number(decimals = 0) |>
    tab_style(style = cell_text(weight = 500),
              locations = cells_column_labels()) |>
    tab_source_note(source_note = md("**Severity** is based on percentile compared to GMFCS matched peers")) |>
    tab_source_note(source_note = md("**Scores** are raw values out of 100")) |>
    tab_style(style = cell_text(size = "x-small"),
              locations = cells_source_notes()) |>
    tab_style(style = cell_text(align = "left", weight = "bold"),
              locations = cells_title()) |>
    # cols_width(name ~ px(220),
    #            value ~ px(100),
    #            qtl ~ px(100)) |>
    data_color(
      columns = sev,
      target_columns = c(name, value, qtl),
      palette = pal,
      na_color = "white"
    )
  
  return(t)
  
}

# ggplot(data = goaltot_ref,
#        mapping = aes(x = value,
#                      y = GMFCS,
#                      color = GMFCS)) +
#   ggdist::stat_pointinterval(
#     point_interval = "mean_qi",
#     .width = c(.5, .9),
#     point_size = 1.6,
#     interval_size_range = c(.5, 1.5),
#     stroke = 1
#   ) +
#   geom_vline(
#     data = goaltot_pt,
#     aes(xintercept = value),
#     linetype = "dashed",
#     linewidth = .3,
#     color = "grey30"
#   ) +
#   geom_point(data = goaltot_pt,
#              size = 2,
#              color = "grey10") +
#   facet_wrap( ~ name) +
#   scale_color_discrete_sequential(palette = "Reds 3", l1 = 30, l2 = 80) +
#   guides(color = "none")
