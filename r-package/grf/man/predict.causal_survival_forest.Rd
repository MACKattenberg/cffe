% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/causal_survival_forest.R
\name{predict.causal_survival_forest}
\alias{predict.causal_survival_forest}
\title{Predict with a causal survival forest forest}
\usage{
\method{predict}{causal_survival_forest}(
  object,
  newdata = NULL,
  num.threads = NULL,
  estimate.variance = FALSE,
  ...
)
}
\arguments{
\item{object}{The trained forest.}

\item{newdata}{Points at which predictions should be made. If NULL, makes out-of-bag
predictions on the training set instead (i.e., provides predictions at
Xi using only trees that did not use the i-th training example). Note
that this matrix should have the number of columns as the training
matrix, and that the columns must appear in the same order.}

\item{num.threads}{Number of threads used in training. If set to NULL, the software
automatically selects an appropriate amount.}

\item{estimate.variance}{Whether variance estimates for hat{tau}(x) are desired
(for confidence intervals).}

\item{...}{Additional arguments (currently ignored).}
}
\value{
Vector of predictions along with optional variance estimates.
}
\description{
Gets estimates of tau(X) using a trained causal survival forest.
}
\examples{
\donttest{
# Train a causal survival forest targeting a Restricted Mean Survival Time (RMST)
# with maxium follow-up time set to `horizon`.
n <- 2000
p <- 5
X <- matrix(runif(n * p), n, p)
W <- rbinom(n, 1, 0.5)
horizon <- 1
failure.time <- pmin(rexp(n) * X[, 1] + W, horizon)
censor.time <- 2 * runif(n)
# Discretizing continuous events decreases runtime.
Y <- round(pmin(failure.time, censor.time), 2)
D <- as.integer(failure.time <= censor.time)
cs.forest <- causal_survival_forest(X, Y, W, D, horizon = horizon)

# Predict using the forest.
X.test <- matrix(0.5, 10, p)
X.test[, 1] <- seq(0, 1, length.out = 10)
cs.pred <- predict(cs.forest, X.test)

# Predict on out-of-bag training samples.
cs.pred <- predict(cs.forest)

# Compute a doubly robust estimate of the average treatment effect.
average_treatment_effect(cs.forest)

# Compute the best linear projection on the first covariate.
best_linear_projection(cs.forest, X[, 1])

# Train a causal survival forest targeting an absolute risk difference
# at the median timepoint `horizon`.
cs.forest.prob <- causal_survival_forest(X, Y, W, D, target = "survival.probability", horizon = 0.5)
}

}
