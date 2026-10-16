library(mgcvUtils)

near <- function(x, y, tol = 1e-8) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
}

# A finite standard error must not overflow or underflow just because its
# square is outside the double-precision range. Use the exact expression
# for the implemented Gaussian variance expansion, not exact lognormal moments.
f <- clognorm(base = "e")
f$putTheta(0)
for (log_mean in c(-400, 0, 400)) {
  p <- f$predict(f, se = TRUE, X = matrix(1), beta = log_mean - 0.5,
    Vb = matrix(0.001))
  stopifnot(is.finite(p$fit), is.finite(p$se.fit), p$se.fit > 0)
  near(p$fit / exp(log_mean), 1.0005)
  near(p$se.fit / exp(log_mean), sqrt(0.001 + 1.5 * 0.001^2))
}

# Exercise every variance component with censoring and verify that changing
# only a prediction offset scales the mean and SE equally, even when g0^2
# would overflow or underflow.
set.seed(42)
d <- data.frame(x = seq(0, 1, length.out = 100))
y <- 1 + d$x + rnorm(nrow(d), sd = 0.3)
d$y <- cbind(pmax(y, 1.2), ifelse(y < 1.2, -Inf, y))
d$o <- 0
b <- gam(y ~ s(x, k = 6) + offset(o), data = d,
  family = clognorm(base = "e"), method = "REML")
nd <- data.frame(x = c(0.2, 0.5, 0.8), o = 0)
p0 <- predict(b, nd, type = "response", se.fit = TRUE)
for (shift in c(-400, 400)) {
  nd$o <- shift
  p <- predict(b, nd, type = "response", se.fit = TRUE)
  stopifnot(all(is.finite(unlist(p))), all(p$se.fit > 0))
  near(p$fit / exp(shift), p0$fit)
  near(p$se.fit / exp(shift), p0$se.fit)
}
