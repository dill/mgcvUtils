library(mgcvUtils)

near <- function(x, y, tol = 1e-8) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
}

set.seed(42)
d <- data.frame(x = seq(0, 1, length.out = 100))
d$o <- 0.2 + 0.1 * sin(2 * pi * d$x)
y <- 1 + d$x + d$o + rnorm(nrow(d), sd = 0.3)

for (base in list("e", 2, 10)) for (nc in c(0L, 20L)) {
  censored <- rank(y) <= nc
  limit <- if (nc) sort(y)[nc] else min(y)
  d$y <- cbind(ifelse(censored, limit, y), ifelse(censored, -Inf, y))
  b <- gam(y ~ s(x, k = 6) + offset(o), data = d,
    family = clognorm(base = base), method = "REML")
  nd <- data.frame(x = seq(0, 1, length.out = 13), o = 0)
  zero <- predict(b, nd, type = "response", se.fit = TRUE)
  # Known offsets multiply every posterior moment of the transformed mean.
  nd$o <- seq(-0.3, 0.7, length.out = nrow(nd))
  shifted <- predict(b, nd, type = "response", se.fit = TRUE)
  multiplier <- (if (identical(base, "e")) exp(1) else base)^nd$o
  near(shifted$fit, zero$fit * multiplier)
  near(shifted$se.fit, zero$se.fit * multiplier)
  stopifnot(all(is.finite(unlist(shifted))))
  near(predict(b, nd, type = "response"), shifted$fit)
  blocked <- predict(b, nd, type = "response", se.fit = TRUE, block.size = 3)
  near(blocked$fit, shifted$fit)
  near(blocked$se.fit, shifted$se.fit)

  # No supplied offset, a scalar zero and a vector of zeros are equivalent.
  X <- predict(b, nd, type = "lpmatrix")
  direct <- function(off = NULL) b$family$predict(b$family, se = TRUE,
    X = X, beta = coef(b), Vb = b$Vp, off = off)
  absent <- direct()
  for (off in list(0, rep(0, nrow(nd)))) {
    ans <- direct(off)
    near(ans$fit, absent$fit)
    near(ans$se.fit, absent$se.fit)
  }
  scalar <- direct(0.4)
  vector <- direct(rep(0.4, nrow(nd)))
  near(scalar$fit, vector$fit)
  near(scalar$se.fit, vector$se.fit)
}

# With no censoring, use mgcv's link prediction as an independent reference
# for its offset semantics, including the separate fitting offset argument.
d$y <- cbind(y, y)
for (in_formula in c(TRUE, FALSE)) {
  b <- if (in_formula) {
    gam(y ~ s(x, k = 6) + offset(o), data = d,
      family = clognorm(base = "e"), method = "REML")
  } else {
    gam(y ~ s(x, k = 6), offset = o, data = d,
      family = clognorm(base = "e"), method = "REML")
  }
  for (newdata in list(NULL, data.frame(x = d$x, o = d$o + 0.5))) {
    args <- list(object = b, se.fit = TRUE)
    if (!is.null(newdata)) args$newdata <- newdata
    link <- do.call(predict, c(args, list(type = "link")))
    response <- do.call(predict, c(args, list(type = "response")))
    q <- link$se.fit^2
    g <- exp(link$fit + b$family$getTheta(TRUE)^2 / 2)
    near(response$fit, g * (1 + q / 2))
    near(response$se.fit, g * sqrt(q + 1.5 * q^2))
  }
}
