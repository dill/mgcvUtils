library(mgcvUtils)

near <- function(x, y, tol = 1e-7) {
  stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
}

# Independent likelihood derivatives via truncated-normal moment integrals.
# Integrating Hermite polynomials gives derivatives of the interval probability;
# convert these to derivatives of its negative logarithm.
derivatives <- function(bounds, mu, sigma) {
  lo <- (bounds[1] - mu) / sigma
  hi <- (bounds[2] - mu) / sigma
  H <- list(function(z) rep(1, length(z)), function(z) z,
    function(z) z^2 - 1, function(z) z^3 - 3*z,
    function(z) z^4 - 6*z^2 + 3)
  moments <- vapply(H, function(h) integrate(function(z) h(z)*dnorm(z),
    lo, hi, rel.tol = 1e-10, abs.tol = 1e-12)$value, numeric(1))
  a <- moments[-1] / moments[1] / sigma^(1:4)
  c(T = -(a[3] - 3*a[2]*a[1] + 2*a[1]^3),
    Q = -(a[4] - 4*a[3]*a[1] - 3*a[2]^2 +
      12*a[2]*a[1]^2 - 6*a[1]^4))
}

set.seed(73)
n <- 120
for (weighted in c(FALSE, TRUE)) for (kind in c("left", "right", "interval", "mixed")) {
  w <- if (weighted) rep(c(0.5, 1, 2, 4), length.out = n) else rep(1, n)
  latent <- rnorm(n, 0.2, 0.6 / sqrt(w))
  y <- cbind(latent, latent)
  types <- if (kind == "mixed") rep(c("left", "right", "interval"), length.out = n) else rep(kind, n)
  left <- types == "left" & latent < 0
  right <- types == "right" & latent > 0.5
  interval <- types == "interval"
  y[left, 1] <- 0
  y[left, 2] <- -Inf
  y[right, 1] <- 0.5
  y[right, 2] <- Inf
  y[interval, 1] <- floor(latent[interval] / 0.4) * 0.4
  y[interval, 2] <- y[interval, 1] + 0.4
  d <- data.frame(w = w)
  d$y <- y
  b <- gam(y ~ 1, data = d, weights = w,
    family = clognorm(base = "e"), method = "REML")
  mu <- unname(coef(b))
  sigma <- b$family$getTheta(TRUE)
  cens <- which(y[,1] != y[,2])
  ds <- vapply(cens, function(i) derivatives(sort(y[i,]), mu, sigma/sqrt(w[i])), numeric(2))
  T <- sum(ds[1,]); Q <- sum(ds[2,])
  V <- b$Vp[1,1]
  # Unit-precision prediction target; training precision enters T, Q and V.
  g <- exp(mu + sigma^2/2)
  a <- V*g
  b2 <- V*(2*g*a - T*a^2)
  expected_mean <- g + (V*g - T*V*a)/2
  expected_var <- g^2*V + (V*(g-T*a))^2/2 - V*(Q*a^2+T*b2-2*g*a)/2
  p <- predict(b, type = "response", se.fit = TRUE)
  stopifnot(all(is.finite(unlist(p))))
  near(p$fit, rep(expected_mean, n))
  near(p$se.fit^2, rep(expected_var, n))
  # Fitting still agrees with cnorm, independently of response prediction.
  ref <- gam(y ~ 1, data = d, weights = w, family = cnorm(), method = "REML")
  near(coef(b), coef(ref))
  near(sigma, ref$family$getTheta(TRUE))

  # Interval endpoint ordering does not change the likelihood or correction.
  if (any(interval)) {
    d$y[interval,] <- d$y[interval,2:1]
    reversed <- gam(y ~ 1, data = d, weights = w,
      family = clognorm(base = "e"), method = "REML")
    pr <- predict(reversed, type = "response", se.fit = TRUE)
    near(pr$fit, p$fit)
    near(pr$se.fit, p$se.fit)
  }
}
