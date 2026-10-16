library(mgcvUtils)

near <- function(x, y, tol = 2e-7) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
}

# Independent derivatives: integrate the normal density and its first four
# location derivatives over the observed interval, then differentiate log(P).
interval_derivatives <- function(bounds, mu, theta, wt) {
  sigma <- exp(theta) / sqrt(wt)
  lo <- (min(bounds) - mu) / sigma
  hi <- (max(bounds) - mu) / sigma
  H <- list(function(z) rep(1, length(z)), function(z) z,
    function(z) z^2 - 1, function(z) z^3 - 3*z,
    function(z) z^4 - 6*z^2 + 3)
  integrals <- vapply(H, function(h) integrate(function(z) h(z)*dnorm(z),
    lo, hi, rel.tol = 1e-12, abs.tol = 1e-13)$value, numeric(1))
  a <- integrals[-1] / integrals[1] / sigma^(1:4)
  c(deviance = -2*log(integrals[1]), Dmu = -2*a[1],
    Dmu2 = -2*(a[2] - a[1]^2),
    Dmu3 = -2*(a[3] - 3*a[2]*a[1] + 2*a[1]^3),
    Dmu4 = -2*(a[4] - 4*a[3]*a[1] - 3*a[2]^2 +
      12*a[2]*a[1]^2 - 6*a[1]^4))
}

# At both endpoint orientations, compare every location/scale derivative with
# integrals, using finite differences in log(sigma) for the scale derivatives.
# Rows mix interval, uncensored, left- and right-censored observations to catch
# replacement-length/recycling errors that a scalar example would miss.
y <- c(0, -1, 0.3, -0.2, 0.7, 0, -0.4, 1, -1, -sqrt(2), -sqrt(3))
censor <- c(1, 0, 0.3, -Inf, Inf, 2, 0, 1.5, 1, sqrt(2), sqrt(3))
attr(y, "censor") <- censor
mu <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
wt <- c(0.5, 2, 1.5, 0.8, 3, 1, 4, 0.7, 1.3^2, 1.3^2, 1.3^2)
theta <- log(1.3)
family <- clognorm(base = "e")
original_dd <- mgcv::cnorm()$Dd
compat <- getFromNamespace(".clognorm_cnorm_compat", "mgcvUtils")
# Force the affected-version path independently of the installed mgcv version.
family$Dd <- compat(original_dd, version = package_version("1.9-4"))
old_warn <- getOption("warn")
options(warn = 2)
result <- family$Dd(y, mu, theta, wt, level = 2)
stopifnot(all(is.finite(unlist(result))))
for (i in which(is.finite(censor) & censor != y)) {
  reference <- function(th) interval_derivatives(c(y[i], censor[i]), mu[i], th, wt[i])
  h <- 0.001
  r <- reference(theta)
  plus <- reference(theta + h); minus <- reference(theta - h)
  plus2 <- reference(theta + 2*h); minus2 <- reference(theta - 2*h)
  first <- (minus2 - 8*minus + 8*plus - plus2) / (12*h)
  second <- (-plus2 + 16*plus - 30*r + 16*minus - minus2)/(12*h^2)
  for (nm in c("Dmu", "Dmu2", "Dmu3", "Dmu4")) near(result[[nm]][i], r[nm])
  near(result$Dth[i], first["deviance"])
  near(result$Dth2[i], second["deviance"], tol = 2e-6)
  near(result$Dmuth[i], first["Dmu"])
  near(result$Dmu2th[i], first["Dmu2"])
  near(result$Dmu3th[i], first["Dmu3"])
  near(result$Dmuth2[i], second["Dmu"], tol = 2e-6)
  near(result$Dmu2th2[i], second["Dmu2"], tol = 2e-6)
}

# All derivative levels, vector versus scalar calls, and reversed endpoint order.
for (level in 0:2) {
  vector <- family$Dd(y, mu, theta, wt, level)
  for (i in seq_along(y)) {
    yi <- y[i]; attr(yi, "censor") <- censor[i]
    scalar <- family$Dd(yi, mu[i], theta, wt[i], level)
    for (nm in names(vector)) near(vector[[nm]][i], scalar[[nm]])
  }
  revy <- y; finite <- is.finite(censor)
  revy[finite] <- censor[finite]
  attr(revy, "censor")[finite] <- y[finite]
  reversed <- family$Dd(revy, mu, theta, wt, level)
  for (nm in names(vector)) near(vector[[nm]], reversed[[nm]])
}

# The local patch leaves ordinary cnorm derivatives unchanged when neither
# endpoint is at the mean; compare all outputs, including scale derivatives.
ordinary_mu <- mu + 0.17
for (level in 0:2) {
  original <- mgcv::cnorm()$Dd(y, ordinary_mu, theta, wt, level)
  corrected <- family$Dd(y, ordinary_mu, theta, wt, level)
  for (nm in names(original)) near(corrected[[nm]], original[[nm]], tol = 1e-12)
}
options(warn = old_warn)

# Newer releases retain their callback, including partially patched development
# callbacks: the compatibility policy is explicitly limited to <= 1.9-4.
for (version in c("1.9-5", "1.9-6", "2.0-0")) {
  stopifnot(identical(compat(original_dd, version = package_version(version)), original_dd))
}
# Applying the same local repair twice is idempotent.
stopifnot(identical(compat(family$Dd, version = package_version("1.9-4")), family$Dd))
stopifnot(identical(body(mgcv::cnorm()$Dd), body(original_dd)))
stopifnot(identical(environment(family$Dd), environment(original_dd)))

# Signed log differences also cover cancellation and zero polynomial terms.
ddnorm <- getFromNamespace(".clognorm_ddnorm", "mgcvUtils")
zero <- ddnorm(-1, 1, s0 = 0, s1 = 0)
stopifnot(identical(as.numeric(zero), -Inf), attr(zero, "sign") == 0)
equal <- ddnorm(-1, 1)
stopifnot(identical(as.numeric(equal), -Inf), attr(equal, "sign") == 0)
large <- ddnorm(-1, 2, a0 = 1000 - dnorm(-1, log = TRUE),
  a1 = 999 - dnorm(2, log = TRUE))
near(large, 1000 + log1p(-exp(-1)), tol = 1e-13)
stopifnot(attr(large, "sign") == -1)
