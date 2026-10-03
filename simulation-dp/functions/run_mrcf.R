# run a cfdd
run_mrcf <- function(
    dt,
    X_names = paste0("X", 1:10),
    stub = y_stub,
    stub_yhat = "yhat_kaplam",
    values_kappa_lambda = values_kaplam){
  
  dt.in <- copy(dt)
  
  # rescale treatment variable
  dt.in[, what := est_hats(target = "posttreat",
                           data = dt.in,
                           byvar_i = "i",
                           byvar_t = "t",
                           byvar_t_star = "t_star")]
  
  cfdds <- as.data.table(lapply(
    values_kappa_lambda,
    function(i) {
      
      # re-scale y-variables for cfdd
      var <- paste0("y_kaplam", i)
      dt.in[, newvar  := est_hats(target = var,
                                  data = dt.in,
                                  byvar_i = "i",
                                  byvar_t = "t",
                                  byvar_t_star = "t_star")]
      setnames(dt.in, "newvar", paste0("yhat_kaplam", i))
      
      # select y-variable
      yvar <- paste0(stub, i)
      print(yvar)
      Y <- as.matrix(dt.in[, ..yvar])
      
      # select variables to center the CF: yhatkaplam, what
      yhatvar <- paste0(stub_yhat, i)
      yhat <- as.matrix(dt.in[, ..yhatvar])
      what <- as.matrix(dt.in[, .(what)])
      
      # run the CF
      cf <- causal_forest(
        X = as.matrix(dt.in[, ..X_names]),
        Y =Y,
        W = dt.in$posttreat,
        W.hat = what,
        Y.hat = yhat,
        seed = 23082019,
        num.trees = N_trees,
        min.node.size =  min_node_size,
        clusters=dt.in$i
      )
      
      # predict and rename yhat
      yhat <- predict(cf, estimate.variance = TRUE)
      yhat <- yhat[, c("predictions", "variance.estimates")]
      colnames(yhat) <- paste0(c("tau_hat_mrcf_", "var_hat_mrcf_"), i)
      
      return(yhat)
    }))
  
  # merge predictions to dt
  res <- cbind(dt.in[, .(i, t, t_star)], cfdds)
  t_star_periods <- unique(dt.in[, t_star])
  cutoff <- t_star_periods[(length(t_star_periods)/2) + 1]
  res <- res[t_star >= cutoff]
  
  return(res)
}