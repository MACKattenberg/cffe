make_data <- function(
    n_individuals = N,
    n_periods = 4,
    first_treat_period = 3,
    n_exogenous = 10,
    kappas = kappa_list,
    lambdas = lambda_list,
    distr = set_distr,
    seed = 21042016) {
  
  set.seed(seed)
  # Data that is constant for a given individual over the periods
  
  # make time-invariant  X_i for one year
  if (distr == "normal") {
    input_X <- rnorm(n_individuals * (n_exogenous + 1))
    #input_epsilon <- rnorm(n_individuals * n_periods)
  }
  
  if (distr == "uniform") {
    input_X <- runif(n_individuals * (n_exogenous + 1), min = -1, max = 1)
    #input_epsilon <- runif(n_individuals * n_periods, min = -1 , max = 1)
  }
  
  X <- matrix(input_X,
              n_individuals,
              n_exogenous + 1)
  constant_data <- data.table(X)
  colnames(constant_data) <- c(paste0("X", 1:n_exogenous), "fe")
  
  
  constant_data[, i := 1:n_individuals]
  constant_data[, p_treat := exp(X1) / (exp(X1) + exp(X2))]
  constant_data[, treat := rbinom(.N, 1, p_treat)]

  # copy constant data for each time period
  data <- rbindlist(
    lapply(
      1:n_periods,
      function(j) {
        dt_out <- copy(constant_data)
        dt_out[, "t" := j]})
  )
  
  # make time-variant variables: post and posttreat
  #data[, epsilon := input_epsilon]
  data[, post := as.numeric(t >= first_treat_period)]
  data[, posttreat := post * treat]
  data[, t_star := t - 2]
  
  # define treatment effect
  cutoff <- data[post ==1, min(t_star)]
  data[, forcing_var := X1 * ifelse(t_star >= cutoff,
                                    t_star,
                                    0)]
  data[, treat_effect := 5 * pmax(forcing_var, 0)]
  
  # make y variables
  data[, yvar  := treat_effect * posttreat + X1^2 + X2^2]
  for (k in kappas) {
    for (l in lambdas) {
      
      if (distr == "normal") epsilon <- rnorm(n_individuals * n_periods)
      if (distr == "uniform") epsilon <- runif(n_individuals * n_periods, 
                                                     min = -1 , max = 1)
      data[, new_y := yvar + k * (fe + treat) - l * t + epsilon]
      setnames(data, "new_y", paste0("y_kaplam", k, l))
    }
  }
  
  # time FE varies with X1
  for (k in kappas) {
    for (l in lambdas) {
      data[, new_y := yvar + k * (fe + treat) - l * t * X1]
      setnames(data, "new_y", paste0("y_kaplam_hett", k, l))
    }
  }
  data[, yvar  := NULL] # clean up
  
  # make event id
  data[, event_id := 1]
  
  
  return(data)
}