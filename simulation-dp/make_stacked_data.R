make_stacked_data <- function(
    n_individuals = N * 4 ,
    n_periods = 10,
    first_treat_period1 = 3,
    first_treat_period2 = 7,
    n_exogenous = 10,
    kappas = kappa_list,
    lambdas = lambda_list,
    distr = set_distr,
    seed = 21042016) {
  
  set.seed(seed)
  
  # Data that is constant for a given individual over the periods
  if (distr == "normal") {
    input_X <- rnorm(n_individuals * (n_exogenous + 1))
    #input_epsilon <- rnorm(n_individuals * n_periods)
  }
  
  if (distr == "uniform") {
    input_X <- runif(n_individuals * (n_exogenous + 1), min = -1, max = 1)
    #input_epsilon <- runif(n_individuals * n_periods, min = -1 , max = 1)
  }
  
  # make time-invariant  X_i for one year
  X <- matrix(input_X,
              n_individuals,
              n_exogenous + 1)
  constant_data <- data.table(X)
  colnames(constant_data) <- c(paste0("X", 1:n_exogenous), "fe")
  
  constant_data[, i := 1:n_individuals]
  constant_data[, p_treat := exp(X1) / (exp(X1) + exp(X2))]
  constant_data[, treat1 := rbinom(.N, 1, p_treat)]
  constant_data[, treat2 := rbinom(.N, 1, p_treat)]
  
  # drop obs that are treated in periods 1 and 2
  sel <- constant_data$treat1 == 1 & constant_data$treat2 == 1
  constant_data <- constant_data[sel == 0]
  
  # randomly assign obs no-treated into control2 group (or in control1)
  N_control <- sum(constant_data$treat1 == 0 & constant_data$treat2 == 0)
  constant_data[, control1 :=
                  ifelse(treat1 == 0 & treat2 == 0,
                         rbinom(N_control, 1, 0.5),
                         0)]
  constant_data[, control2 :=
                  ifelse(treat1 == 0 & treat2 == 0 & control1 == 0,
                         1,
                         0)]
  # add time-variant variables: data
  data <- rbindlist(
    lapply(
      1:n_periods,
      function(j) {
        dt_out <- copy(constant_data)
        dt_out[, "t" := j]})
  )
  
  # make time-variant variables: post and posttreat
  #data[, epsilon := input_epsilon[1:.N]]
  data[, post1 := as.numeric(t >= first_treat_period1)]
  data[, posttreat1 := post1 * treat1]
  data[, post2 := as.numeric(t >= first_treat_period2)]
  data[, posttreat2 := post2 * treat2]
  
  # selects obs with t_star >= -2
  data[, t_star :=
         ifelse(treat1 == 1 | control1 == 1,
                t - first_treat_period1,
                t)]
  data[, t_star :=
         ifelse(treat2 == 1 | control2 == 1,
                t - first_treat_period2,
                t_star)]
  
  # make raw data: drop obs receiving first treatment when second treatment commences
  data_raw <- copy(data)
  data_raw[, to_drop := posttreat1 == 1 & t < first_treat_period2]
  data_raw <- data_raw[to_drop == 0,]
  data_raw[, to_drop := NULL]
  
  # make stacked DD data
  sel <- data$t_star >= -2 & data$t_star <= 1
  data <- data[sel,]
  
  #define treatment effects
  cutoff <- data[post1 == 1, min(t_star)] # cutoff applies to events 1 and 2
  data[, forcing_var := X1 * (t_star + 1)]
  data[, treat_effect1 := 10 * pmax(forcing_var, 0)]
  data[, treat_effect2 := 2 * treat_effect1]
  
  
  # make y variables
  data[, yvar  := treat_effect1 * posttreat1 + treat_effect2 * posttreat2 + 
         X2^2  + X1^2]
  for (k in kappas) {
    for (l in lambdas) {
      
      if (distr == "normal") epsilon <- rnorm(nrow(data))
      if (distr == "uniform") epsilon <- runif(nrow(data), min = -1 , max = 1)

      data[, new_y := yvar + k * (fe + treat1 + treat2) - l * t + epsilon]
      setnames(data, "new_y", paste0("y_kaplam", k, l))
    }
  }
  
  data[, yvar  := NULL] # clean up
  
  # make indicators posttreat and obs sampled for sec_treatment
  data[, posttreat := posttreat1 + posttreat2]
  data[, treat := treat1 + treat2]
  data[, sec_treatment:= treat2 + control2]
  
  
  # adjust data such that t start at one
  # (which is adjusted to zero in the wrapper run_CFEF)
  min_t <- min(data$t)
  if (min_t < 1) {
    data[, t := t + abs(min_t) + 1]
  }
  if (min_t > 1) {
    data[, t := t -  min_t + 1]
  }
  
  # adjust data such that t_star start at one
  # (which is adjusted to zero in the wrapper run_CFEF)
  min_t_star <- min(data$t_star)
  if (min_t_star < 1) {
    data[, t_star := t_star + abs(min_t_star) + 1]
  }
  if (min_t_star > 1) {
    data[, t_star := t_star -  min_t_star + 1]
  }
  
  # adjust data such that i starts at 1 and there are no "missing ids"
  setnames(data, "i", "i_orig")
  setorder(data, "i_orig")
  N <- length(unique(data$i_orig))
  data$i <- rep(1:(N),1, each = length(unique(data$t_star)))
  
  # keep only one treatment indicator 
  data[, postttreat := ifelse(sec_treatment == 1, posttreat2, posttreat1)]
  data[, post := ifelse(sec_treatment == 1, post2, post1)]
  data[, treat := ifelse(sec_treatment == 1, treat2, treat1)]
  data[, control := ifelse(sec_treatment == 1, control2, control1)]
  data[, treat_effect := ifelse(sec_treatment == 1, treat_effect2, treat_effect1)]
  sel <- c(grep("*1", colnames(data), value= TRUE),
           grep("*2", colnames(data), value= TRUE))
  sel <- setdiff(sel, c("X1", "X2", "X10"))
  data[, (sel) := NULL]
  
  return(data)
}