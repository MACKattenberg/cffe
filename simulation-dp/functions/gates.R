# make a GATES plot
gates <- function(dt_ols,
                  dt_effects,
                  N_groups = 10,
                  forcing_var = "tau_hat_cffe_55",
                  depvar = paste0(y_stub, "55"),
                  treatment = "posttreat",
                  time_fe = "t",
                  indiv_fe = "i",
                  event_time_fe = "t_star",
                  truth = "treat_effect"){
  
  
  # rename variables in effects
  effects <- copy(dt_effects)
  sel_cols <- c(c(time_fe, indiv_fe, event_time_fe, forcing_var))
  setnames(effects, sel_cols, c("time", "indiv", "event_time", "fv"))
  
  # compute decile based on mean of forcing var per individu
  effects[, mean_fv := mean(fv), by = indiv] # compute mean
  effects[, group:= as.numeric(cut(
    mean_fv,
    quantile(mean_fv, probs = seq(0, 1, 0.1), na.rm=TRUE),
    include.lowest = TRUE),
    labels = FALSE)]
  
  # compute mean and semean by decile
  effects[, dec_mean_fv := mean(fv), by = group]
  effects[, N := 1]
  effects[, Ngroup := sum(N), by = group]
  effects[, dec_sd_fv  := sd(fv), by = group] 
  effects[, dec_semean_fv  := dec_sd_fv / sqrt(Ngroup)] 
  effects[, dec_mean_fv_lb := dec_mean_fv - 1.96 * dec_semean_fv]
  effects[, dec_mean_fv_ub := dec_mean_fv + 1.96 * dec_semean_fv]
  
  # select estimates of fv by decile
  out_fv <- effects[, .(group, dec_mean_fv, dec_mean_fv_lb, dec_mean_fv_ub)]
  out_fv <- unique(out_fv)
  setnames(out_fv, c("group", "mean_fv", "lb_fv", "ub_fv"))
  
  # add overall estimates
  out_fv <- rbind(
    out_fv,
    list(group = 0,
         mean_fv = effects[, mean(fv)],
         lb_fv = effects[, mean(fv) - 1.96 * sd(fv)/sqrt(.N)],
         ub_fv = effects[, mean(fv) + 1.96 * sd(fv)/sqrt(.N)]))
  
  setorderv(out_fv, "group")
  
  
  # rename variables in ols data
  ols <- copy(dt_ols)
  setnames(
    ols,
    c(depvar, treatment, time_fe, indiv_fe, event_time_fe, truth),
    c("y", "T_post", "time", "indiv", "event_time","truth"))
  
  # merge decile indicator to ols data
  dim(ols)
  ols <- merge(
    x = ols, 
    y = effects[, .(indiv, time, group)], 
    by = c("indiv", "time"),
    all = TRUE
  )
  
  # update group indicator in ex_ante periods (it is only register in post period)
  ols[, group := mean(group, na.rm =TRUE), by = "indiv"]
  
  # compute overall effect
  lm <- feols(y ~ T_post | time + indiv + event_time, vcov = "twoway", data = ols)
  att <- as.list(c(
    "group" = 0,
    lm$coeftable[, c("Estimate","Std. Error")],
    "truth" = mean(ols[post == 1, truth])))
  
  
  # compute effect by decile
  catt <- rbindlist(lapply(
    sort(unique(ols$group)),
    function(i) {
      lm <- feols(y ~ T_post | time + indiv + event_time, vcov = "twoway",
                  data = ols[group == i])
      catt <- as.list(c(
        "group" = i,
        lm$coeftable[, c("Estimate","Std. Error")],
        "truth" = mean(ols[post == 1 & group == i, truth])))
    }
  ))
  
  # collect results OLS
  res <- rbind(att, catt)
  setnames(res, c("Estimate", "Std. Error"), c("ols_beta", "ols_se"))
  
  # merge results from effects
  res <- merge(res, out_fv, by = "group")
  
  return(res)
}