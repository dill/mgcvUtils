#' Fix $cdf() slot of a mgcv family
#' 
#' @description Fix $cdf() slot of a mgcv family.
#' 
#' @param fam an \code{mgcv} family
#' 
#' @examples
#' library(gamUtils); library(MASS)
#' b <- gam(list(accel~s(times,k=20,bs="ad"),~s(times)),
#'          data=mcycle,family=gaulss())
#' 
#' b$family <- fix_family_cdf( b$family )
#' 
#' # Quantile residuals should look uniformly distributed
#' qres <- b$family$cdf(mcycle$accel, b$fitted.values,
#'                      wt = rep(1, nrow(mcycle)), scale = 1)
#' hist(qres)
#' # Not too bad
#' @importFrom stats pnorm pgamma pbinom ppois
#' @export
#'
fix_family_cdf <- function(fam) {
  
  ## Try if mgcv provides $cdf slot...
  if( !is.null(fam$cdf) ) {
    return( fam )
  }
  
  ## ... if not provide gamUtils' version
  fnam <- paste0(".cdf.", fam$family)
  
  fam$cdf <- get(fnam, mode = "function", envir = asNamespace("gamUtils"))
  
  fam  
}


