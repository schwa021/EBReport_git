
ii <- sample(nrow(mod$X), 300)

x <- mod$X[ii,]
y <- predict(mod, x)
xale <- as_tibble(bind_cols(y, x))
names(xale)[1] <- "resp"


predbart <- function(object, newdata) {
  x <- newdata[names(object$X)]
  predict(object = object, new_data = x, verbose = F)
}


aleout <- 
  ale::ale(
  data = xale,
  model = mod,
  pred_fun = predbart,
  y_col = "resp"
)



remove_stats <- function(x, stat_type) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$stat)[1] == stat_type
                     })
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}

remove_geoms <- function(x, geom_type) {
  # Find layers that match the requested type.
  selector <- sapply(x$layers,
                     function(y) {
                       class(y$geom)[1] == geom_type
                     })
  # Delete the layers.
  x$layers[selector] <- NULL
  x
}

p <- aleout$plots$meansta_Hip_Ang_Trn

p <-remove_geoms(p, "GeomRug")
