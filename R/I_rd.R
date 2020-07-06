

# Internal to gamUtils?
.rd.gevlss <- function(mu, wt = NULL, scale = NULL) {
  out <- mu[ , 1] + exp(mu[ , 2]) * 
    ((-log(runif(nrow(mu))))^(-mu[ , 3])-1) / mu[ , 3]
  return( out )
}