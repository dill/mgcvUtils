
# Internal to gamUtils?
.qf.gaulss <- function(p, mu, wt = NULL, scale = NULL) {
  out <- qnorm(p, mu[ , 1, drop = TRUE], 1 / mu[ , 2, drop = TRUE])
  return( out )
}


.qf.gaussian <-function(p, mu, wt = NULL, scale = NULL) {
  out <- qnorm(p, mu, sqrt(scale/wt))
  return( out )
}
