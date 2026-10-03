# dynamic causal forest
run_dcf <- function(dt,
                    X_names = paste0("X", 1:10),
                    y_stub =  "y_kaplam",
                    kaplam = values_kaplam,
                    treatment_id,
                    seed = 18062014){
  
  dt <- copy(dt)
  # define base_time as second period for each treatment id
  dt[, base_time := min(t) + 1, by = treatment_id]
  
  # only keep observations at or after base time
  dt <- dt[t >= base_time,]
  
  # make dynamic outcome variables (z)
  y_vars <- paste0(y_stub, kaplam)
  sel_rows <- which(dt$t == dt$base_time)
  sel_cols <- c("i", y_vars)
  dt_base <- dt[sel_rows, ..sel_cols]
  setnames(dt_base, y_vars, paste0("z_kaplam", kaplam))      
  
  dt <- merge(dt, dt_base, by = "i", all.x = TRUE)
  
  for (kl in kaplam){
    y <- paste0(y_stub, kl)
    setnames(dt, paste0("z_kaplam", kl), "z")
    y_var <- dt[, ..y]
    dt[, z := y_var - z]
    setnames(dt, "z", paste0("z_kaplam", kl))
  }
  
  # select periods other than base
  periods <- setdiff(unique(dt$t), unique(dt$base_time))
  
  # select transformed y variables
  z_vars <- grep("z_kaplam", colnames(dt), value = TRUE)
  
  # make grid with depvars and time and  run run_cf by period and depvar
  grid <- expand.grid(periods, z_vars)
  colnames(grid) <- c("period", "depvar")
  
  run_cf <- function(t, z_var){
    
    print(paste("period:", t, "depvar:", z_var))
    
    # select data in period t
    sel_rows <- which(dt$t == t)
    dt_cf <- dt[sel_rows]
    
    # make matrices for y,W and X
    X <- as.matrix(dt_cf[, ..X_names])
    W <- as.matrix(dt_cf[, treat])
    y <- as.matrix(dt_cf[, ..z_var])
    
    # run a regular CF
    dcf <- causal_forest(
      X = X,
      Y = y,
      W = W,
      num.trees = N_trees,
      clusters = dt_cf$i -1,
      seed = seed)
    
    # predict and rename yhat
    yhat <- predict(dcf, estimate.variance = TRUE)
    yhat <- yhat[, c("predictions", "variance.estimates")]
    colnames(yhat) <- paste0(c("tau_hat_dcf_", "var_hat_dcf_"),  
                             stringr::str_sub(z_var,-2,-1))
    
    # make result data with i, t, t_star and tau_hat
    res <- as.data.table(cbind(
      dt_cf$i,
      t,
      dt_cf$t_star,
      paste0("tau_hat_dcf_", stringr::str_sub(z_var,-2,-1)),
      yhat))
    
    setnames(res, 
             c("i",
               "t",
               "t_star",
               "estimate",
               "value_estimate",
               "value_var"))
    setcolorder(res, c("i", "t", "estimate", "value_estimate", "value_var"))
    
    return(res)
  }
  
  # apply run_cf to each row of combination of period and depvar
  res <- rbindlist(lapply(
    1:nrow(grid),
    function(i) {
      run_cf(t = grid[i, "period"], z_var = grid[i, "depvar"][[1]])
    }))
  
  # rearrange the data
  res <- dcast(res, i + t + t_star ~ estimate, value.var = c("value_estimate", "value_var"))
  vars <- colnames(res)
  res[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
  
  # assign proper names
  colnames(res) <- gsub("value_estimate_tau", "tau", colnames(res))
  colnames(res) <- gsub("value_var_tau", "var", colnames(res))
  return(res)
}
