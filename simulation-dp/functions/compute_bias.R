# function to compute rmse
compute_bias <- function(dt, y, yhats){
  unlist(lapply(yhats,
                function(i) mean((dt[, ..y][[1]] - dt[, ..i][[1]]), na.rm = TRUE)))
}
