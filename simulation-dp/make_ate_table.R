# function to make ATE table
make_ate_table <- function(
    dt,
    yhats,
    varhats,
    output_folder,
    truth = "treat_effect",
    save_xls = "ate_table.xlsx",
    save_tex = "ate_table.tex",
    return = c("var", "$\\kappa$", "$\\lambda$", "mean", "lb", "ub")){
  
  # check numer of yhats equals number of varhats
  stopifnot(length(yhats) == length(varhats))
  
  # copy data
  dt.in <- copy(dt)
  
  
  # make draws by lapplying sample_N_draws over elements in yhats and varhats
  draws <- rbindlist(lapply(
    1:length(varhats),
    function(i) {
      sample_N_draws(N = 100,
                     dt = dt.in,
                     mean_var = yhats[i],
                     var_var = varhats[i])}))
    
  # compute the measures for each estimator
  res <- compute_measures(draws)
  
  # add truth
  res <- rbind(
    list("var" = "truth",
         "$\\kappa$" = NA,
         "$\\lambda$" = NA,
         "mean" = round(mean(dt[, ..truth][[1]]), 3),
         "sd" = NA,
         "lb" = NA,
         "median" = NA,
         "ub" = NA),
    res)
  
  # make required selection
  res <- res[, ..return]
  

  # save results
  #setnames(res, c("kappa", "lambda"), c("\u03BA", "\u03BB"))
  write_xlsx(res, paste0(output_folder, save_xls))
  
  print(xtable(res),
        sanitize.colnames.function = identity,
        include.rownames = FALSE,
        file = paste0(output_folder, save_tex),
        floating = FALSE)
  
  return(res)
}


# sample observations from a normal distribution with mean yhats and var varhats
sample_N_draws <- function(N = 500, dt, mean_var, var_var){
  draws <- rbindlist(lapply(
    1:N,
    function(i){
      res <- rnorm(nrow(dt),
                   mean = dt[, ..mean_var][[1]],
                   sd = sqrt(dt[, ..var_var][[1]]))
      res <- data.table("value" = res,
                        "draw" = i,
                        "id" = 1:length(res),
                        "var" = mean_var)
    }))
  
  # organize results
  res <- dcast(draws, id + var ~ draw, value.var = "value")
  res[, id := NULL]
  setnames(res, c("var", paste0("run", 1:(ncol(res) - 1))))
  
  return(res)
}


# helper function that computes the measures given draws and estimator
compute_measures <- function(dt_draws, estimator = "var", digits = 3){
  sel_cols <- setdiff(colnames(dt_draws), estimator)  
  
  # compute the mean of each run by estimator
  res <- dt_draws[, lapply(.SD, mean, na.rm=TRUE), by = estimator, .SDcols = sel_cols ]
  
  # compute measures: i.e the mean of means, lb of means, etc.
  means <- round(rowMeans(res[, ..sel_cols]), digits)
  sds <- round(matrixStats::rowSds(as.matrix(res[, ..sel_cols])), digits)
  lb <- round(matrixStats::rowQuantiles(as.matrix(res[, ..sel_cols]),
                                        probs=c(0.025)), digits)
  median <- round(matrixStats::rowQuantiles(as.matrix(res[, ..sel_cols]),
                                            probs=c(0.5)), digits)
  ub <- round(matrixStats::rowQuantiles(as.matrix(res[, ..sel_cols]),
                                        probs=c(0.975)), digits)
  
  # make additional columns
  var <- res[, ..estimator][[1]]
  #var <- c("tau_hat_cffe_50", "tau_hat_cffe_55")
  kappa <- stringr::str_sub(var, -2, -2)
  lambda <- stringr::str_sub(var, -1, -1)
  
  # remove lambda, kappa, "tau_hat_" and "_" from var
  for (i in unique(c(kappa, lambda))) var <- gsub(i, "", var)
  var <- gsub("tau_hat_", "", var)
  var <- gsub("_", " ", var)
  var <- toupper(var)
  
  # collect results
  out <- data.table(
    "var" = var,
    "$\\kappa$" = kappa,
    "$\\lambda$" = lambda,
    "mean" = means,
    "sd" = sds,
    "lb" = lb,
    "median" =median,
    "ub" = ub)
  
  return(out)
}
