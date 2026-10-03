# function to compute averages over individuals and time
est_hats <- function(target,
                     data = data,
                     byvar_i = "i",
                     byvar_t = "t",
                     byvar_t_star = "t_star"){
  dt_in <- copy(data)
  setnames(dt_in, target, ".y")
  
  dt_in[, .y_i      := mean(.y), by = byvar_i]
  dt_in[, .y_t      := mean(.y - .y_i), by = byvar_t]
  dt_in[, .y_t_star := mean(.y - .y_i - .y_t), by = byvar_t_star]
  
  dt_in[, .y_hat := .y_i + .y_t + .y_t_star]
  
  # assign final name
  name <- paste0(target, "_hat")
  setnames(dt_in, ".y_hat", name)
  out <- dt_in[, ..name]
}