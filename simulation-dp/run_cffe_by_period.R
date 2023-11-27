# run a cffe stacked by period
run_cffe_by_period <- function(
    dt,
    X_names= paste0("X", 1:10),
    y_names = paste0("y_kaplam", values_kaplam),
    treatment_id,
    individual_fe = "i",
    time_fe = "t",
    event_time_fe = "t_star",
    seed = 18062014){
  
  dt.in <- copy(dt)
  
  #run_cffe_over_kaplam is hard-coded in terms of i,t and t_star for FE
  setnames(dt.in,
           c(treatment_id, individual_fe, time_fe, event_time_fe),
           c("TREAT_ID", "i", "t", "t_star"))
  
  # select treatments events tp make datasets based on treatment id
  treatment_events <- unique(dt.in[, TREAT_ID])
  
  # make a list with each element containing data for a  single treatment event
  # then make separate datasets containing ex-ante periods and one treatment 
  # period per treatment event
  dt_events <- split_dt_on_event(events = treatment_events, dt = dt.in)
  dts <- select_estimation_samples(dt_events)
  dts <- purrr::list_flatten(dts) # convert dts to a unnested list
  
  # run cffe over dts
  res <- rbindlist(lapply(
    dts,
    function(i) run_cffe_over_dt(dt = i,
                                 yvars = y_names,
                                 xvars = X_names,
                                 seed = seed)))
  
  # organize the results
  res <- dcast(res,
             i + t + t_star ~ variable + yvar)
  
  # shorten name of predictions in this application
  if (length(grep("y_kaplam", colnames(res))) > 0) {
    colnames(res) <- gsub("y_kaplam", "", colnames(res))
  }
  
  return(res)
}



# helper function to run cffe over dts
run_cffe_over_dt <- function(dt, yvars, xvars, seed = seed){
  dt.in <- copy(dt)
  
  # given dt.in, run cfee over values of kaplam
  res <- rbindlist(lapply(
    yvars,
    function(y) {
      res <- run_cffe_over_kaplam(yvar = y,
                                  dt = dt.in,
                                  sel_X = xvars,
                                  seed = seed)
      return(res) 
    }))
}

# helper function that runs cffe over kaplam
# returns a dt with 6 columns: i, t, t_star, kaplam, variable (tau or var), value
run_cffe_over_kaplam <- function(yvar, dt, sel_X, seed){
  
  # select y-variable using kaplam
  print(yvar)
  Y <- as.matrix(dt[, ..yvar])
  
  # select posttreat variable and X-matrix
  W <- dt$posttreat
  Xmat <- as.matrix(dt[, ..sel_X])
  
  # run the causal FE forest
  cffe <- causal_fe_forest(
    X = Xmat,
    Y =Y,
    W = W,
    individual_fe = dt$i - 1, # FE indices must start at 0
    time_fe = dt$t - 1,
    event_time_fe = dt$t_star - 1, # FE indices must start at 0
    seed = seed,
    num.trees = N_trees,
    min.node.size = min_node_size,
    clusters = dt$i)
  
  # predict and rename yhat
  yhat <- predict(cffe, estimate.variance = TRUE)
  yhat <- yhat[, c("predictions", "variance.estimates")]
  colnames(yhat) <- c("tau_hat_cffe", "var_hat_cffe")
  yhat$yvar <- yvar
  yhat <- cbind(dt[, .(i, t, t_star)], yhat)
  sel_rows <- which(dt$t == dt[, max(t)])
  yhat <- yhat[sel_rows,]
  res <- melt(as.data.table(yhat), id.vars = c("yvar", "i", "t", "t_star"))
  
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
