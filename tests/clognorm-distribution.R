library(mgcvUtils)

same <- function(x, y) stopifnot(isTRUE(all.equal(x, y)))
mu <- c(-1, 0, 0.5, 2)
wt <- c(0.5, 1, 2, 4)
p <- c(0.01, 0.25, 0.75, 0.99)

for (base in list(10, "e", 2, 0.5)) {
  fam <- clognorm(base = base)
  cc <- if (identical(base, "e")) 1 else log(base)
  for (sigma in c(0.4, 1.7)) {
    fam$putTheta(log(sigma))
    meanlog <- mu * cc
    sdlog <- sigma * abs(cc) / sqrt(wt)
    q <- qlnorm(p, meanlog, sdlog)
    same(fam$qf(p, mu, wt), q)
    same(fam$cdf(q, mu, wt), p)
    same(fam$cdf(q, mu, wt, logp = TRUE), log(p))
    same(fam$qf(p, mu, wt, scale = 99), q)
    same(fam$cdf(q, mu, wt, scale = 99), p)
    set.seed(42)
    expected <- rlnorm(length(mu), meanlog, sdlog)
    set.seed(42)
    same(fam$rd(mu, wt, scale = 99), expected)
    same(fam$qf(p, mu), qlnorm(p, meanlog, sigma * abs(cc)))
    same(fam$qf(c(0, 1), mu = 0), c(0, Inf))
    same(fam$cdf(c(-1, 0, Inf), mu = 0), c(0, 0, 1))
    same(fam$cdf(c(0, Inf), mu = 0, logp = TRUE), c(-Inf, 0))
    stopifnot(is.na(fam$qf(NA_real_, 0)), is.na(fam$cdf(NA_real_, 0)))
  }
  stopifnot(identical(fix_family_rd(fam)$rd, fam$rd),
            identical(fix_family_qf(fam)$qf, fam$qf),
            identical(fix_family_cdf(fam)$cdf, fam$cdf))
}

# Fit censored observations on the log scale used by cnorm, then check that
# all helpers see the fitted theta, including after serialization.
set.seed(52)
x <- seq(0, 1, length.out = 100)
z <- 0.5 + x + rnorm(length(x), sd = 0.3)
y <- cbind(pmax(z, 0.5), ifelse(z < 0.5, -Inf, z))
b <- mgcv::gam(y ~ x, family = clognorm(base = 10), method = "REML")
b <- unserialize(serialize(b, NULL))
fam <- b$family
expected <- qlnorm(0.9, b$fitted.values * log(10),
                   fam$getTheta(TRUE) * log(10))
same(as.vector(quantile(b, probs = 0.9)), as.vector(expected))
same(as.vector(quantile(b, probs = 0.9, newdata = data.frame(x = x))),
     as.vector(expected))
same(fam$cdf(expected, b$fitted.values), rep(0.9, length(x)))
set.seed(23)
expected_draw <- fam$rd(b$fitted.values, b$prior.weights)
set.seed(23)
same(as.vector(simulate(b, nsim = 1, method = "rd")), expected_draw)
set.seed(23)
expected_draw <- fam$qf(runif(length(x)), b$fitted.values, b$prior.weights)
set.seed(23)
same(as.vector(simulate(b, nsim = 1, method = "qf")), expected_draw)

# Left, interval, and right censoring: CDF bounds support randomized
# quantile residuals without treating censoring thresholds as point masses.
lower <- c(0, 1, 2)
upper <- c(1, 2, Inf)
mu_cens <- c(-0.2, 0.1, 0.4)
f_lower <- fam$cdf(lower, mu_cens)
f_upper <- fam$cdf(upper, mu_cens)
stopifnot(f_lower[1] == 0, f_upper[3] == 1, all(f_lower < f_upper))
set.seed(65)
u <- runif(3, f_lower, f_upper)
residuals <- qnorm(u)
latent <- fam$qf(pnorm(residuals), mu_cens)
stopifnot(all(is.finite(residuals)), all(latent > lower), all(latent < upper))
