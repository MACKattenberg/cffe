make_bias_table <- function(
    dt,
    yhats = list(yhats_cffe, yhats_mrcf, yhats_dcf),
    colnames_yhats = c("cffe", "mrcf", "dcf"),
    save_xls,
    save_tex,
    seed = 23082019) {
  
  # helper function to compute bias and CFI bounds
  run_bias_cfi <- function(dt = dt, y = "treat_effect", yhat_vars) {
    
    # compute BIAS in data
    value <- round(compute_bias(dt = dt, y = "treat_effect", yhats = yhat_vars), 4)
    
    # compute lower and upper bound using bootstrapping
    set.seed(seed)
    bootstrapped_bias <- unlist(lapply(
      1:500, 
      function(s) {
        s <- sample(1:nrow(dt), nrow(dt), replace = TRUE)
        bias <- compute_bias(dt = dt[s, ], y = "treat_effect", yhats = yhat_vars)
        return(bias)
      }))
    
    bounds <- round(quantile(bootstrapped_bias, probs = c(0.025, 0.975)), 4)
    
    # collect output
    out <- list("estimate" = yhat_vars,
                "bias" = value,
                "lb" = bounds[1][[1]], 
                "ub" = bounds[2][[1]])
    return(out)
  }
  
  
  # apply helper function to yhats
  res <- rbindlist(lapply(
    yhats,
    function(i) run_bias_cfi(dt = dt, y = "treat_effect", yhat_vars = i)))
 
  
  # make identifying columns
  kappa <- stringr::str_sub(res$estimate, -2, -2)
  lambda <- stringr::str_sub(res$estimate, -1, -1)
  estimator <- res$estimate
  for (v in values_kaplam) estimator <- gsub(v, "", estimator)
  estimator <- gsub("tau_hat_", "", estimator)
  estimator <- gsub("_", " ", estimator)
  estimator <- toupper(estimator)
  cfi <- paste0("(", res$lb, " - ", res$ub, ")")
  
  res <- data.table(
    "estimator"  = estimator, 
    "$\\kappa$"  = kappa , 
    "$\\lambda$" = lambda, 
    "bias"       = res[, bias], 
    "cfi"        = cfi)
  

  write_xlsx(res, paste0(output_folder, save_xls))
  print(xtable(res),
        sanitize.colnames.function = identity,
        include.rownames = FALSE,
        file = paste0(output_folder, save_tex),
        floating = FALSE)
  
  return(res)
}