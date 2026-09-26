library(mgcvUtils)

near <- function(x, y, tol = 1e-8) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
}

set.seed(42)
d <- data.frame(x = seq(0, 1, length.out = 100))
y <- 1 + d$x + rnorm(nrow(d), sd = 0.3)
nd <- data.frame(x = seq(0, 1, length.out = 11))

# Uncensored vectors and equivalent two-column responses give the same fit.
d$y <- y
vector_fit <- gam(y ~ s(x, k = 6), data = d,
  family = clognorm(base = 2), method = "REML")
d$y <- cbind(y, y)
matrix_fit <- gam(y ~ s(x, k = 6), data = d,
  family = clognorm(base = 2), method = "REML")
near(coef(vector_fit), coef(matrix_fit))
near(vector_fit$null.deviance, matrix_fit$null.deviance)
pv <- predict(vector_fit, nd, type = "response", se.fit = TRUE)
pm <- predict(matrix_fit, nd, type = "response", se.fit = TRUE)
near(pv$fit, pm$fit)
near(pv$se.fit, pm$se.fit)

# Exercise the already-initialized representation supplied by fitting paths
# such as bam: a numeric response carrying a censor attribute. Supply the
# same full design matrix for both calls, isolating response normalization
# from bam's separate design-matrix storage issue.
for (nc in c(0L, 1L, 20L)) {
  censored <- rank(y) <= nc
  limit <- if (nc) sort(y)[nc] else min(y)
  response <- cbind(ifelse(censored, limit, y), ifelse(censored, -Inf, y))
  d$y <- response
  model <- gam(y ~ s(x, k = 6), data = d,
    family = clognorm(base = 2), method = "REML")
  X <- predict(model, type = "lpmatrix")
  run_postproc <- function(response) {
    family <- clognorm(base = 2)
    family$putTheta(model$family$getTheta())
    G <- list(X = X)
    pp <- family$postproc(family, response, model$prior.weights,
      model$fitted.values, model$linear.predictors, rep(0, nrow(d)), TRUE)
    pred <- family$predict(family, se = TRUE, X = X,
      beta = coef(model), Vb = model$Vp)
    list(null.deviance = pp$null.deviance, fit = pred$fit, se.fit = pred$se.fit)
  }
  attributed <- response[, 1L]
  attr(attributed, "censor") <- response[, 2L]
  pm <- run_postproc(response)
  pv <- run_postproc(attributed)
  stopifnot(all(is.finite(unlist(pv))))
  near(pv$null.deviance, pm$null.deviance)
  near(pv$fit, pm$fit)
  near(pv$se.fit, pm$se.fit)
}
