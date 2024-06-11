source("SCRIPTS/limbdist.R")

res_control_L <- list()
res_treated_L <- list()
res_control_R <- list()
res_treated_R <- list()

for (s in surglist) {
  cat("Processing", s, "\n")
  
  cat("Left Side\n")
  res_control_L[[s]] <- limbdist(s, datx=dat, xside=xL, status="control")
  res_treated_L[[s]] <- limbdist(s, datx=dat, xside=xL, status="treated")
  
  cat("Right Side\n\n")
  res_control_R[[s]] <- limbdist(s, datx=dat, xside=xL, status="control")
  res_treated_R[[s]] <- limbdist(s, datx=dat, xside=xL, status="treated")
}

pctl_control_L <- sapply(res_control_L, function(x) x$pctl)
pctl_control_R <- sapply(res_control_R, function(x) x$pctl)
pctl_treated_L <- sapply(res_treated_L, function(x) x$pctl)
pctl_treated_R <- sapply(res_treated_R, function(x) x$pctl)

pctl <-
  tibble(
    Surgery = str_replace_all(surglist, "_", " "),
    L_treated = pctl_treated_L,
    R_treated = pctl_treated_R,
    L_control = pctl_control_L,
    R_control = pctl_control_R
  ) %>% 
  mutate(
    across(
      -Surgery, 
      ~ cut(., breaks = c(0, 25, 75, 95, 99, 100), labels = c("Close", "Typical", "Moderate", "Far", "Very Far"))
    )
  )

c <- sequential_hcl(n=11, palette = "Reds 3")[c(11, 10, 6)]
c <- c("grey65", "grey90", c)

pctl %>% 
  gt() %>% 
  tab_header("Proximity to Matches") %>% 
  tab_style(
      cell_text(weight = "bold", align = "left"),
      locations = cells_title()
  ) %>% 
  tab_style(
    cell_text(weight = 500),
    locations = cells_column_labels()
  ) %>% 
  cols_label(
    L_treated = "Left",
    R_treated = "Right",
    L_control = "Left", 
    R_control = "Right"
  ) %>% 
  tab_spanner(
    label = "Treated",
    columns = matches("treated")
  ) %>% 
  tab_spanner(
    label = "Control",
    columns = matches("control")
  ) %>% 
  cols_width(
    2:5 ~ px(110)
  ) %>% 
  data_color(
    columns = c(L_treated, R_treated, L_control, R_control),
    palette = c
  )
