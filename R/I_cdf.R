

.cdf.poisson <- function(q, mu, wt, scale, logp = FALSE) {
  ppois(q, mu, log.p = logp)
}

.cdf.binomial <- function(q, mu, wt, scale, logp = FALSE) {
  pbinom(q * (wt + as.numeric(wt == 0)), wt, mu, log.p = logp)
}

.cdf.Gamma <- function(q, mu, wt, scale, logp = FALSE) {
  pgamma(q, shape = 1/scale, scale = mu * scale, log.p = logp)
} 

.cdf.gaussian <- function(q, mu, wt, scale, logp = FALSE) {
  pnorm(q, mean = mu, sd = sqrt(scale/wt), log.p = logp)
}

.cdf.gaulss <- function(q, mu, wt, scale, logp = FALSE) {
  pnorm(q, mean = mu[ , 1], sd = 1/mu[ , 2], log.p = logp)
}