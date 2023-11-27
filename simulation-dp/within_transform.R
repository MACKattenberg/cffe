within_transform <- function(target,
                             data = data,
                             byvar_i = "i"){
  dt_in <- copy(data)
  setnames(dt_in, target, ".y")
  
  dt_in[, .y_i   := mean(.y), by = byvar_i]
  dt_in[, .y_hat := .y - .y_i]
  
  # assign final name
  name <- paste0(target, "_wt")
  setnames(dt_in, ".y_hat", name)
  out <- dt_in[, ..name]
}