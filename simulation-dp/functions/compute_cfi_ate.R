# function to compute ATEs
compute_cfi_ate <- function(
    dt,
    estimators = c("cffe_period", "cfdd_period", "dcf"),
    truth = "treat_effect",
    values_kaplam = values_kaplam,
    save_xls,
    save_tex){
  # copy data
  dt.in <- copy(dt)
  
  # define function that computes mean and cfi
  # for different values of kaplam given estimator
  run_cfi <- function(values_kaplam, estimator){
    rbindlist(lapply(
      values_kaplam,
      function(i) {
        var <- paste0("tau_hat_", estimator, "_", i)
        yhat <- dt.in[, ..var][[1]]
        
        mean <- mean(yhat)
        res <- data.table(
          "estimator" = estimator,
          "$\\kappa$"  = substr(i, 1, 1),
          "$\\lambda$"  = substr(i, 2, 2),
          "mean" = mean,
          "lb" = mean - 1.96 * sd(yhat) / sqrt(nrow(dt.in)),
          "ub" = mean + 1.96 * sd(yhat) / sqrt(nrow(dt.in)))
        
        return(res)
      }))
  }
  
  # lapply the helper function over estimators
  res <- rbindlist(lapply(
    estimators,
    function(i) run_cfi(values_kaplam = values_kaplam, estimator =i)))
  
  # add row with true ATE
  ATE <- mean(dt.in[, ..truth][[1]])
  res <- rbind(data.table(estimator = "truth",
                          "$\\kappa$"  = "",
                          "$\\lambda$"  = "",
                          "mean" = ATE,
                          "lb" = NA,
                          "ub" = NA),
               res)
  
  # save results
  write_xlsx(res, paste0(output_folder, save_xls))
  print(xtable(res),
        sanitize.colnames.function = identity,
        include.rownames = FALSE,
        file = paste0(output_folder, save_tex),
        floating = FALSE)
  
  return(res)
}