prop_shap_plot <- function(modshap, xpt, s, ptprops){
  
  # Build predictor function -----
  shapmod <- Predictor$new(modshap, data = modshap$X)
  
  
  # Get Shapley values -----
  xx <- 
    xpt %>% 
    select(names(modshap$X)) %>% 
    data.frame() %>% 
    mutate(
      across(
        where(is.numeric),
        ~ signif(., 2)
      )
    )
  
  set.seed(42)
  shap <- Shapley$new(shapmod, x.interest = xx, sample.size = 30)
  
  
  # Make Labels -----
  for (f in shap$results$feature) {
    val <- xx[[f]]
    lab <- varlabs$Label[varlabs$Variable == f]
    lab <- ifelse(is.na(lab), f, lab)
    lab <- str_replace_all(lab, "_", " ")
    labx <- str_wrap(glue("{lab} = {val}"), 35)
    shap$results$label[shap$results$feature == f] <- labx
    shap$results$lab[shap$results$feature == f] <- lab
    shap$results$value[shap$results$feature == f] <- as.character(val)
  }
  
  
  # Arrange data and scale phi based on threshold -----
  temp <- 
    shap$results %>% 
    arrange(desc(phi)) %>% 
    mutate(
      feature.value = str_replace(feature.value, "=", " = "),
      feature.value = str_replace_all(feature.value, "_", " ")
    ) %>% 
    mutate(feature.value = fct_rev(fct_inorder(feature.value))) %>% 
    mutate(label = fct_rev(fct_inorder(label))) %>% 
    mutate(phix = phi * 0.5 / modshap$prob_rule_class) %>% 
    mutate(phix = ifelse(abs(phix > 0.5), sign(phix)*.5, phix))
  
  
  # Get propensity data -----
  # prop_post <- bart_machine_get_posterior(modshap, xpt[names(modshap$X)])$y_hat_posterior_samples
  # pt_prop <- signif(rowSums(prop_post > modshap$prob_rule_class) / 1000, 2)
  pt_prop <- 
    ptprops %>% 
    filter(Surgery == str_replace_all(s, "_", " ")) %>% 
    select(-Surgery) %>% 
    as.numeric()
    
  
  
  tit <- str_replace_all(s, "_", " ")
  tit <- glue("Indications For or Against\n{tit}")
  tit <- str_wrap(tit, 30)
  # xlim <- 1.5 * shap$y.hat.average
  xlim <- .5 
  
  p <- ggplot(temp, aes(x=phix, y=label, fill = phix)) + 
    geom_col() +
    geom_vline(xintercept = 0, linewidth = .5, color = "grey10") +
    labs(title = tit) +
    xlab("") +
    scale_x_continuous(breaks = c(-xlim, xlim), 
                       labels = c("Less Like\nHistorical Standard", "More Like\nHistorical Standard")) +
    ylab("") +
    scale_fill_gradient2(low = "red3", high = "green3", mid="grey90", limits = c(-xlim, xlim)) +
    coord_cartesian(xlim = c(-xlim, xlim)) +
    guides(fill = "none") +
    theme(
      axis.text = element_text(size = 5),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 8, face = "bold")
    )
  
  
  rng <- 1.5 * shap$y.hat.average
  tit <- str_replace_all(s, "_", " ")
  subtit <- glue("Propensity = {pt_prop[2]}% ({pt_prop[1]}%, {pt_prop[3]}%)<br>*Range derived from estimated measurement error*")
  
  mypal <- brewer.pal(5, "RdYlGn")
  pcol <- case_when(
    pt_prop[2] < 20 ~ mypal[1],
    pt_prop[2] >= 20 & pt_prop[2] < 40 ~ mypal[2],
    pt_prop[2] >= 40 & pt_prop[2] < 60 ~ mypal[3],
    pt_prop[2] >= 60 & pt_prop[2] < 80 ~ mypal[4],
    pt_prop[2] >= 80 ~ mypal[5],

        between(pt_prop[2], 20, 40) ~ mypal[2],
    between(pt_prop[2], 40, 60) ~ mypal[3],
    between(pt_prop[2], 60, 80) ~ mypal[4],
    TRUE ~ mypal[4]
  )
  
  tbl <- 
    temp %>% 
    select(phix, lab, value) %>% 
    arrange(desc(phix)) %>% 
    gt() %>% 
    tab_header(
      title = tit,
      subtitle = md(subtit)
    ) %>% 
    opt_align_table_header(align = "left") %>% 
    cols_hide(phix) %>% 
    cols_label(
      lab = "Patient Characteristic",
      value = "Value"
    ) %>% 
    cols_width(
      value ~ px(150)
    ) %>% 
    cols_align(
      align = "center",
      columns = value
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(),
        cells_title(groups = c("title"))
      )
    ) %>% 
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_body(columns = lab)
    ) %>% 
    
      data_color(
      columns = phix,
      method = "bin",
      target_columns = everything(),
      palette = "RdYlGn",
      alpha = .5,
      bins = 5,
      domain = c(-.5, .5),
      autocolor_text = F
    ) %>% 
    opt_table_outline(color = pcol, width = px(4))
  
  return(list("plt" = p, "tbl" = tbl))
  
}
