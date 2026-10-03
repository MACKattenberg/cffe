# run a cfdd stacked by period
run_mrcf_by_period <- function(
    dt,
    X_names = paste0("X", 1:10),
    y_stub = "y_kaplam",
    values_kappa_lambda = values_kaplam,
    treatment_id){
  
  dt <- copy(dt)
  
  # make datasets based on treatment id
  setnames(dt, treatment_id, "TREAT_ID")
  treatment_events <- unique(dt[, TREAT_ID])
  
  # make a list with each element containing data for a  single treatment event
  # then make separate datasets containing ex-ante periods and one treatment 
  # period per treatment event
  dt_events <- split_dt_on_event(events = treatment_events, dt = dt)
  dts <- select_estimation_samples(dt_events)
  dts <- purrr::list_flatten(dts) # convert dts to a unnested list
  
  
  # run cffe over dts
  res <- rbindlist(lapply(
    dts, 
    function(i) run_cfdd_over_dt(dt = i,
                                 xvars = X_names)))
  
  # organize the results
  res <- dcast(res,
               i + t + t_star ~ variable + kaplam)
  return(res)
}



# helper function to run cffe over dts
run_cfdd_over_dt <- function(dt, xvars){
  dt.in <- copy(dt)
  
  # given dt.in, run cfee over values of kaplam
  res <- rbindlist(lapply(
    values_kaplam,
    function(kl) {
      res <- run_cfdd_over_kaplam(kaplam = kl,
                                  dt = dt.in,
                                  sel_X = xvars)
      return(res) 
    }))
}

# helper function that runs cffe over kaplam
# returns a dt with 6 columns: i, t, t_star, kaplam, variable (tau or var), value
run_cfdd_over_kaplam <- function(kaplam,
                                 dt,
                                 sel_X){
  
  # rescale treatment variable
  dt[, what := est_hats(target = "posttreat",
                        data = dt,
                        byvar_i = "i",
                        byvar_t = "t",
                        byvar_t_star = "t_star")]
  
  # re-scale y-variable
  var <- paste0(y_stub, kaplam)
  dt[, newvar  := est_hats(target = var,
                           data = dt,
                           byvar_i = "i",
                           byvar_t = "t",
                           byvar_t_star = "t_star")]
  setnames(dt, "newvar", paste0("yhat_kaplam", kaplam))
  
  # select y-variable
  yvar <- paste0(y_stub, kaplam)
  print(yvar)
  Y <- as.matrix(dt[, ..yvar])
  
  # select variables to center the CF: yhat_kaplam, what
  yhatvar <- paste0("yhat_kaplam", kaplam)
  yhat <- as.matrix(dt[, ..yhatvar])
  what <- as.matrix(dt[, .(what)])
  
  Xmat <- as.matrix(dt[, ..sel_X])
  # run the CF
  cf <- causal_forest(
    X = Xmat,
    Y =Y,
    W = dt$posttreat,
    W.hat = what,
    Y.hat = yhat,
    seed = 18062014,
    num.trees = N_trees,
    min.node.size =  min_node_size,
    clusters=dt$i
  )
  
  # predict and rename yhat
  yhat <- predict(cf, estimate.variance = TRUE)
  yhat <- yhat[, c("predictions", "variance.estimates")]
  colnames(yhat) <- c("tau_hat_mrcf", "var_hat_mrcf")
  yhat$kaplam <- kaplam
  yhat <- cbind(dt[, .(i, t, t_star)], yhat)
  yhat <- yhat[t == max(t),]
  res <- melt(as.data.table(yhat),
              id.vars = c("kaplam", "i", "t", "t_star"))
  
  return(res)
}

split_dt_on_event <- function(events, dt){
  lapply(
    events,
    function(event_i) {
      dt[TREAT_ID == event_i,]
    })
}


select_estimation_samples <- function(dt){
  out <- lapply(
    1:length(dt),
    function(element) {
      
      # select the element to restructure
      dt.in <- copy(dt[[element]])
      
      # select cutoff time periods
      periods <- unique(dt.in$t)
      cutoff <- periods[length(periods)/2]
      print(paste("assuming t indicates periods and the last ante period is:", cutoff))
      
      # select data with ex-ante numbers
      dt_ante <- dt.in[t <= cutoff]
      
      # select data with ex-post periods and rbind dt_ante
      post_periods <- periods[which(periods > cutoff)]
      dt.out <- lapply(
        post_periods,
        function(period_id){
          dt.out <- rbind(dt_ante, dt.in[t == period_id,])
          dt.out <- setorderv(dt.out, cols = c("i", "t"))
        })
      
      return(dt.out)
    })
  
  return(out)
}


