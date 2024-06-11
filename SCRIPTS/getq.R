getq <- function(x, sd){
  qsd <- 
    x |>
    filter(SIDE == sd) |> 
    select(starts_with("p_")) |> 
    reframe(
      across(
        .cols = everything(),
        ~ 100 * round(quantile(., probs = c(.05, .50, .95)), 2)
      )
    ) |> 
    mutate(
      SIDE = sd,
      q = c("q5", "q50", "q95")
    ) |> 
    pivot_longer(-c(SIDE, q)) |> 
    pivot_wider(names_from = q, values_from = value) |> 
    mutate(name = str_replace_all(str_remove_all(name, "p_"), "_", " ")) |> 
    rename(Surgery = name)
}