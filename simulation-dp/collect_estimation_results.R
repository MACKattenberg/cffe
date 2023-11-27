collect_estimation_results <- function(
    dt,
    y_vars = c("y_kaplam00", "y_kaplam50", "y_kaplam05", "y_kaplam55"),
    select_from_dt = NULL,
    estimates){
  
  sel <- c(
    c("i", "t", "t_star", "forcing_var", "treat_effect", "posttreat"),
    select_from_dt)
  
  effects <- dt[, ..sel]
  effects <- cbind(effects, data[t >= 3, ..y_vars])
  
  for (est in estimates){
    effects <- merge(effects, est, by = c("i", "t", "t_star"), all.x = TRUE)
  }
  
  return(effects)
}
