# run a cffe
run_cffe <- function(
    dt,
    X_names = paste0("X", 1:10),
    stub =  "y_kaplam",
    values_kappa_lambda = values_kaplam){
  
  # run the cffe stacked for all combinations in values_kappa_lambda
  cfefs <- as.data.table(lapply(
    values_kaplam,
    function(i) {
      
      # select y-variable
      yvar <- paste0(stub, i)
      print(yvar)
      Y <- as.matrix(dt[, ..yvar])
      
      # run the causal FE forest
      cffe <- causal_fe_forest(
        X = as.matrix(dt[, ..X_names]),
        Y =Y,
        W = dt$posttreat,
        individual_fe = dt$i - 1, # FE indices must start at 0
        time_fe = dt$t - 1,
        event_time_fe = dt$t_star - 1, # FE indices must start at 0
        seed = 23082019,
        num.trees = N_trees,
        min.node.size = min_node_size,
        clusters=dt$i)
      
      # predict and rename yhat
      yhat <- predict(cffe, estimate.variance = TRUE)
      yhat <- yhat[, c("predictions", "variance.estimates")]
      colnames(yhat) <- paste0(c("tau_hat_cffe_", "var_hat_cffe_"), i)
      
      return(yhat)
    }))
  
  # merge predictions to dt
  res <- dt[, .(i, t, t_star)]
  res <- cbind(res, cfefs)
  t_star_periods <- unique(dt$t_star)
  cutoff <- t_star_periods[(length(t_star_periods)/2) + 1]
  res <- res[t_star >= cutoff]
}