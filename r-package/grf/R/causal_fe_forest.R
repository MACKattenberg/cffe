#' Causal forest
#'
#' Trains a causal forest that can be used to estimate
#' conditional average treatment effects tau(X). When
#' the treatment assignment W is binary and unconfounded,
#' we have tau(X) = E[Y(1) - Y(0) | X = x], where Y(0) and
#' Y(1) are potential outcomes corresponding to the two possible
#' treatment states. When W is continuous, we effectively estimate
#' an average partial effect Cov[Y, W | X = x] / Var[W | X = x],
#' and interpret it as a treatment effect given unconfoundedness.
#'
#' @param X The covariates used in the causal regression.
#' @param Y The outcome (must be a numeric vector with no NAs).
#' @param W The treatment assignment (must be a binary or real numeric vector with no NAs).
#' @param Y.hat Estimates of the expected responses E[Y | Xi], marginalizing
#'              over treatment. If Y.hat = NULL, these are estimated using
#'              a separate regression forest. See section 6.1.1 of the GRF paper for
#'              further discussion of this quantity. Default is NULL.
#' @param W.hat Estimates of the treatment propensities E[W | Xi]. If W.hat = NULL,
#'              these are estimated using a separate regression forest. Default is NULL.
#' @param num.trees Number of trees grown in the forest. Note: Getting accurate
#'                  confidence intervals generally requires more trees than
#'                  getting accurate predictions. Default is 2000.
#' @param sample.weights Weights given to each sample in estimation.
#'                       If NULL, each observation receives the same weight.
#'                       Note: To avoid introducing confounding, weights should be
#'                       independent of the potential outcomes given X. Default is NULL.
#' @param clusters Vector of integers or factors specifying which cluster each observation corresponds to.
#'  Default is NULL (ignored).
#' @param equalize.cluster.weights If FALSE, each unit is given the same weight (so that bigger
#'  clusters get more weight). If TRUE, each cluster is given equal weight in the forest. In this case,
#'  during training, each tree uses the same number of observations from each drawn cluster: If the
#'  smallest cluster has K units, then when we sample a cluster during training, we only give a random
#'  K elements of the cluster to the tree-growing procedure. When estimating average treatment effects,
#'  each observation is given weight 1/cluster size, so that the total weight of each cluster is the
#'  same. Note that, if this argument is FALSE, sample weights may also be directly adjusted via the
#'  sample.weights argument. If this argument is TRUE, sample.weights must be set to NULL. Default is
#'  FALSE.
#' @param sample.fraction Fraction of the data used to build each tree.
#'                        Note: If honesty = TRUE, these subsamples will
#'                        further be cut by a factor of honesty.fraction. Default is 0.5.
#' @param mtry Number of variables tried for each split. Default is
#'             \eqn{\sqrt p + 20} where p is the number of variables.
#' @param min.node.size A target for the minimum number of observations in each tree leaf. Note that nodes
#'                      with size smaller than min.node.size can occur, as in the original randomForest package.
#'                      Default is 5.
#' @param honesty Whether to use honest splitting (i.e., sub-sample splitting). Default is TRUE.
#'  For a detailed description of honesty, honesty.fraction, honesty.prune.leaves, and recommendations for
#'  parameter tuning, see the grf algorithm reference.
#' @param honesty.fraction The fraction of data that will be used for determining splits if honesty = TRUE. Corresponds
#'                         to set J1 in the notation of the paper. Default is 0.5 (i.e. half of the data is used for
#'                         determining splits).
#' @param honesty.prune.leaves If TRUE, prunes the estimation sample tree such that no leaves
#'  are empty. If FALSE, keep the same tree as determined in the splits sample (if an empty leave is encountered, that
#'  tree is skipped and does not contribute to the estimate). Setting this to FALSE may improve performance on
#'  small/marginally powered data, but requires more trees (note: tuning does not adjust the number of trees).
#'  Only applies if honesty is enabled. Default is TRUE.
#' @param alpha A tuning parameter that controls the maximum imbalance of a split. Default is 0.05.
#' @param imbalance.penalty A tuning parameter that controls how harshly imbalanced splits are penalized. Default is 0.
#' @param stabilize.splits Whether or not the treatment should be taken into account when
#'                         determining the imbalance of a split. Default is TRUE.
#' @param ci.group.size The forest will grow ci.group.size trees on each subsample.
#'                      In order to provide confidence intervals, ci.group.size must
#'                      be at least 2. Default is 2.
#' @param tune.parameters A vector of parameter names to tune.
#'  If "all": all tunable parameters are tuned by cross-validation. The following parameters are
#'  tunable: ("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
#'   "honesty.prune.leaves", "alpha", "imbalance.penalty"). If honesty is FALSE the honesty.* parameters are not tuned.
#'  Default is "none" (no parameters are tuned).
#' @param tune.num.trees The number of trees in each 'mini forest' used to fit the tuning model. Default is 200.
#' @param tune.num.reps The number of forests used to fit the tuning model. Default is 50.
#' @param tune.num.draws The number of random parameter values considered when using the model
#'                          to select the optimal parameters. Default is 1000.
#' @param compute.oob.predictions Whether OOB predictions on training set should be precomputed. Default is TRUE.
#' @param num.threads Number of threads used in training. By default, the number of threads is set
#'                    to the maximum hardware concurrency.
#' @param seed The seed of the C++ random number generator.
#'
#' @return A trained causal forest object. If tune.parameters is enabled,
#'  then tuning information will be included through the `tuning.output` attribute.
#'
#' @examples
#' \donttest{
#' # Train a causal forest.
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
#' c.forest <- causal_forest(X, Y, W)
#'
#' # Predict using the forest.
#' X.test <- matrix(0, 101, p)
#' X.test[, 1] <- seq(-2, 2, length.out = 101)
#' c.pred <- predict(c.forest, X.test)
#'
#' # Predict on out-of-bag training samples.
#' c.pred <- predict(c.forest)
#'
#' # Predict with confidence intervals; growing more trees is now recommended.
#' c.forest <- causal_forest(X, Y, W, num.trees = 4000)
#' c.pred <- predict(c.forest, X.test, estimate.variance = TRUE)
#'
#' # In some examples, pre-fitting models for Y and W separately may
#' # be helpful (e.g., if different models use different covariates).
#' # In some applications, one may even want to get Y.hat and W.hat
#' # using a completely different method (e.g., boosting).
#' n <- 2000
#' p <- 20
#' X <- matrix(rnorm(n * p), n, p)
#' TAU <- 1 / (1 + exp(-X[, 3]))
#' W <- rbinom(n, 1, 1 / (1 + exp(-X[, 1] - X[, 2])))
#' Y <- pmax(X[, 2] + X[, 3], 0) + rowMeans(X[, 4:6]) / 2 + W * TAU + rnorm(n)
#'
#' forest.W <- regression_forest(X, W, tune.parameters = "all")
#' W.hat <- predict(forest.W)$predictions
#'
#' forest.Y <- regression_forest(X, Y, tune.parameters = "all")
#' Y.hat <- predict(forest.Y)$predictions
#'
#' forest.Y.varimp <- variable_importance(forest.Y)
#'
#' # Note: Forests may have a hard time when trained on very few variables
#' # (e.g., ncol(X) = 1, 2, or 3). We recommend not being too aggressive
#' # in selection.
#' selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.2)
#'
#' tau.forest <- causal_forest(X[, selected.vars], Y, W,
#'   W.hat = W.hat, Y.hat = Y.hat,
#'   tune.parameters = "all"
#' )
#' tau.hat <- predict(tau.forest)$predictions
#' }
#'
#' @export
causal_fe_forest <- function(X, Y, W, individual_fe, time_fe, event_time_fe = NULL,
                          Y.hat = NULL,
                          W.hat = NULL,
                          num.trees = 2000,
                          sample.weights = NULL,
                          clusters = NULL,
                          equalize.cluster.weights = FALSE,
                          sample.fraction = 0.5,
                          mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                          min.node.size = 5,
                          honesty = TRUE,
                          honesty.fraction = 0.5,
                          honesty.prune.leaves = TRUE,
                          alpha = 0.05,
                          imbalance.penalty = 0,
                          stabilize.splits = TRUE,
                          ci.group.size = 2,
                          tune.parameters = "none",
                          tune.num.trees = 200,
                          tune.num.reps = 50,
                          tune.num.draws = 1000,
                          compute.oob.predictions = TRUE,
                          num.threads = NULL,
                          seed = runif(1, 0, .Machine$integer.max)) {
  if (is.null(event_time_fe)) {
    event_time_fe <- rep(1, length(time_fe))
  }
						  
  has.missing.values <- validate_X(X, allow.na = TRUE)
  validate_sample_weights(sample.weights, X)
  Y <- validate_observations(Y, X)
  W <- validate_observations(W, X)
  individual_fe <- validate_observations(individual_fe, X)
  time_fe <- validate_observations(time_fe, X)
  event_time_fe <- validate_observations(event_time_fe, X)
  clusters <- validate_clusters(clusters, X)
  samples.per.cluster <- validate_equalize_cluster_weights(equalize.cluster.weights, clusters, sample.weights)
  num.threads <- validate_num_threads(num.threads)

  all.tunable.params <- c("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
                          "honesty.prune.leaves", "alpha", "imbalance.penalty")
  default.parameters <- list(sample.fraction = 0.5,
                             mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                             min.node.size = 5,
                             honesty.fraction = 0.5,
                             honesty.prune.leaves = TRUE,
                             alpha = 0.05,
                             imbalance.penalty = 0)

  args.orthog <- list(X = X,
                      num.trees = max(50, num.trees / 4),
                      sample.weights = sample.weights,
                      clusters = clusters,
                      equalize.cluster.weights = equalize.cluster.weights,
                      sample.fraction = sample.fraction,
                      mtry = mtry,
                      min.node.size = 5,
                      honesty = TRUE,
                      honesty.fraction = 0.5,
                      honesty.prune.leaves = honesty.prune.leaves,
                      alpha = alpha,
                      imbalance.penalty = imbalance.penalty,
                      ci.group.size = 1,
                      tune.parameters = tune.parameters,
                      num.threads = num.threads,
                      seed = seed)

  if (is.null(Y.hat)) {
    forest.Y <- do.call(regression_forest, c(Y = list(Y), args.orthog))
    Y.hat <- predict(forest.Y)$predictions
  } else if (length(Y.hat) == 1) {
    Y.hat <- rep(Y.hat, nrow(X))
  } else if (length(Y.hat) != nrow(X)) {
    stop("Y.hat has incorrect length.")
  }

  if (is.null(W.hat)) {
    forest.W <- do.call(regression_forest, c(Y = list(W), args.orthog))
    W.hat <- predict(forest.W)$predictions
  } else if (length(W.hat) == 1) {
    W.hat <- rep(W.hat, nrow(X))
  } else if (length(W.hat) != nrow(X)) {
    stop("W.hat has incorrect length.")
  }

  Y.centered <- Y - Y.hat
  W.centered <- W - W.hat
  data <- create_train_matrices(X, outcome = Y.centered, treatment = W.centered,
                                individual_fe = individual_fe, time_fe = time_fe,
								event_time_fe = event_time_fe, sample.weights = sample.weights)
  args <- list(num.trees = num.trees,
               clusters = clusters,
               samples.per.cluster = samples.per.cluster,
               sample.fraction = sample.fraction,
               mtry = mtry,
               min.node.size = min.node.size,
               honesty = honesty,
               honesty.fraction = honesty.fraction,
               honesty.prune.leaves = honesty.prune.leaves,
               alpha = alpha,
               imbalance.penalty = imbalance.penalty,
               stabilize.splits = stabilize.splits,
               ci.group.size = ci.group.size,
               compute.oob.predictions = compute.oob.predictions,
               num.threads = num.threads,
               seed = seed,
               reduced.form.weight = 0)

  tuning.output <- NULL
  if (!identical(tune.parameters, "none")) {
    if (identical(tune.parameters, "all")) {
      tune.parameters <- all.tunable.params
    } else {
      tune.parameters <- unique(match.arg(tune.parameters, all.tunable.params, several.ok = TRUE))
    }
    if (!honesty) {
      tune.parameters <- tune.parameters[!grepl("honesty", tune.parameters)]
    }
    tune.parameters.defaults <- default.parameters[tune.parameters]
    tuning.output <- tune_forest(data = data,
                                 nrow.X = nrow(X),
                                 ncol.X = ncol(X),
                                 args = args,
                                 tune.parameters = tune.parameters,
                                 tune.parameters.defaults = tune.parameters.defaults,
                                 tune.num.trees = tune.num.trees,
                                 tune.num.reps = tune.num.reps,
                                 tune.num.draws = tune.num.draws,
                                 train = causal_fe_train)

    args <- utils::modifyList(args, as.list(tuning.output[["params"]]))
  }

  forest <- do.call.rcpp(causal_fe_train, c(data, args))
  class(forest) <- c("causal_fe_forest", "grf")
  forest[["ci.group.size"]] <- ci.group.size
  forest[["X.orig"]] <- X
  forest[["Y.orig"]] <- Y
  forest[["W.orig"]] <- W
  forest[["individual_fe.orig"]] <- individual_fe
  forest[["time_fe.orig"]] <- time_fe
  forest[["event_time_fe.orig"]] <- event_time_fe
  forest[["Y.hat"]] <- Y.hat
  forest[["W.hat"]] <- W.hat
  forest[["clusters"]] <- clusters
  forest[["equalize.cluster.weights"]] <- equalize.cluster.weights
  forest[["sample.weights"]] <- sample.weights
  forest[["tunable.params"]] <- args[all.tunable.params]
  forest[["tuning.output"]] <- tuning.output
  forest[["has.missing.values"]] <- has.missing.values

  forest
}

#' @export
predict.causal_fe_forest <- function(object, newdata = NULL,
                                  num.threads = NULL,
                                  estimate.variance = FALSE, ...) {
  # If possible, use pre-computed predictions.
  if (is.null(newdata) && !estimate.variance && !is.null(object$predictions)) {
    return(data.frame(predictions = object$predictions))
  }

  num.threads <- validate_num_threads(num.threads)
  forest.short <- object[-which(names(object) == "X.orig")]
  X <- object[["X.orig"]]
  individual_fe <- object[["individual_fe.orig"]]
  time_fe <- object[["time_fe.orig"]]
  event_time_fe <- object[["event_time_fe.orig"]]
  Y.centered <- object[["Y.orig"]] - object[["Y.hat"]]
  W.centered <- object[["W.orig"]] - object[["W.hat"]]
  train.data <- create_train_matrices(X, outcome = Y.centered, treatment = W.centered, individual_fe = individual_fe, time_fe = time_fe, event_time_fe = event_time_fe)

   args <- list(forest.object = forest.short,
                num.threads = num.threads,
                estimate.variance = estimate.variance)

   if (!is.null(newdata)) {
     validate_newdata(newdata, X, allow.na = FALSE)
     test.data <- create_test_matrices(newdata)
     ret <- do.call.rcpp(causal_fe_predict, c(train.data, test.data, args))
   } else {
     ret <- do.call.rcpp(causal_fe_predict_oob, c(train.data, args))
   }

  # Convert list to data frame.
  empty <- sapply(ret, function(elem) length(elem) == 0)
  do.call(cbind.data.frame, ret[!empty])
}
