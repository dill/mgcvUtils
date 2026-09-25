library(mgcvUtils)

near <- function(x, y, tol = 1e-8) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
}

# Positive responses keep the existing z calculation finite. This test targets
# metadata transport, not the separate question of the response/log scale.
set.seed(42)
d <- data.frame(x = seq(0, 1, length.out = 100))
y <- 1 + d$x + rnorm(nrow(d), sd = 0.3)
make_response <- function(nc) {
  censored <- rank(y) <= nc
  limit <- if (nc) sort(y)[nc] else min(y)
  cbind(ifelse(censored, limit, y), ifelse(censored, -Inf, y))
}

# Independent scalar reference for an intercept-only model. With X = 1,
# the third/fourth likelihood tensors reduce to sums of scalar derivatives.
reference <- function(model, response) {
  sigma <- model$family$getTheta(TRUE)
  mu <- unname(coef(model)[1L])
  V <- model$Vp[1L, 1L]
  c <- log(attr(model$family, "base"))
  censored <- response[, 2L] == -Inf
  # Match the current postprocessor's z definition, deliberately unchanged.
  z <- (log(response[censored, 1L], base = attr(model$family, "base")) - mu) / sigma
  lambda <- exp(dnorm(z, log = TRUE) - pnorm(z, log.p = TRUE))
  T <- sum(lambda * ((z + lambda) * (z + 2 * lambda) - 1)) / sigma^3
  Q <- -sum(lambda * (-(z + lambda)^2 * (z + 2 * lambda) +
    2 * (z + lambda) + (z + 2 * lambda) -
    lambda * (z + lambda) * ((z + 2 * lambda) + 2 * (z + lambda)))) / sigma^4
  g <- exp(c * mu + c^2 * sigma^2 / 2)
  g1 <- c * g
  g2 <- c^2 * g
  g3 <- c^3 * g
  a <- V * g1
  b2 <- V * (2 * g2 * a - T * a^2)
  list(
    fit = g + (V * g2 - T * V * a) / 2,
    se.fit = sqrt(g1^2 * V + (V * (g2 - T * a))^2 / 2 -
      V * (Q * a^2 + T * b2 - 2 * g3 * a) / 2),
    gaussian.fit = g * (1 + c^2 * V / 2)
  )
}

for (nc in c(0L, 1L, 20L)) {
  d$y <- make_response(nc)
  b <- gam(y ~ 1, data = d, family = clognorm(base = 2), method = "REML")
  got <- predict(b, newdata = d[1L, , drop = FALSE], type = "response", se.fit = TRUE)
  want <- reference(b, d$y)
  near(got$fit, want$fit)
  near(got$se.fit, want$se.fit)
  stopifnot(all(is.finite(unlist(got))))
  if (nc > 0L) stopifnot(abs(got$fit - want$gaussian.fit) > 1e-8)
}

# Exercise multiple coefficients and all censoring subset dimensions.
for (nc in c(0L, 1L, 20L)) {
  d$y <- make_response(nc)
  b <- gam(y ~ s(x, k = 6), data = d, family = clognorm(base = 2), method = "REML")
  nd <- data.frame(x = seq(0, 1, length.out = 13))
  before <- predict(b, nd, type = "response", se.fit = TRUE)
  stopifnot(all(is.finite(unlist(before))))
  near(predict(b, nd, type = "response"), before$fit)
  blocked <- predict(b, nd, type = "response", se.fit = TRUE, block.size = 3)
  near(blocked$fit, before$fit)
  near(blocked$se.fit, before$se.fit)

  # A fresh family instance must not overwrite an earlier fit's metadata.
  other <- gam(y ~ x, data = d, family = clognorm(base = 10), method = "REML")
  after <- predict(b, nd, type = "response", se.fit = TRUE)
  near(after$fit, before$fit)
  near(after$se.fit, before$se.fit)

  # The shared environment must survive serialization of a fitted model.
  restored <- unserialize(serialize(b, NULL))
  after <- predict(restored, nd, type = "response", se.fit = TRUE)
  near(after$fit, before$fit)
  near(after$se.fit, before$se.fit)
}
