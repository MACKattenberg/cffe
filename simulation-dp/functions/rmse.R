# function to compute rmse
rmse <- function(dt, y, yhats){
  unlist(lapply(yhats,
                function(i) sqrt(mean((dt[, ..y][[1]] - dt[, ..i][[1]])^2, na.rm = TRUE))))
}