# collapse data for a diff-in-diff plot
convert_to_dd_plot <- function(dt, target, treat = "treat"){
  dt.in <- copy(dt)
  setnames(dt.in, target, "yvar")
  
  # compute counterfactual outcomes in abcence of treatment
  dt.in[, cfl := yvar - posttreat * treat_effect]
  
  # compute mean of target and counterfactual
  cols <- c("yvar", "cfl")
  out <- melt(
    dt.in[, lapply(.SD, mean , na.rm = TRUE), by = c("t", treat), .SDcols = cols],
    id.vars = c("t", treat))
  out[variable == "yvar", variable := target]
  
  return(out)
}