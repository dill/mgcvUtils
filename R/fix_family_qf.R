#' Fix $qf() slot of a mgcv family
#' 
#' @description Fix $qf() slot of a mgcv family.
#' 
#' @param fam an \code{mgcv} family
#' 
#' @examples
#' library(mgcv);library(MASS)
#' b <- gam(list(accel~s(times,k=20,bs="ad"),~s(times)),
#'          data=mcycle,family=gaulss())
#' 
#' b$family <- fix_family_qf( b$family )
#' qfit <- sapply(c(0.1, 0.5, 0.9),
#'                function(.q)
#'                  b$family$qf(.q, b$fitted.values))
#' 
#' plot(mcycle, ylim = c(-150, 100))
#' for(ii in 1:ncol(qfit)){
#'   lines(mcycle$times, qfit[ , ii], col = 2)
#' }
#' 
#' @importFrom mgcv fix.family.qf
#' @export
#'
fix_family_qf <- function(fam) {
  
  fam <- mgcv::fix.family.qf(fam)
  
  ## Try if mgcv provides $qf slot...
  if( !is.null(fam$qf) ) {
    return( fam )
  }
  
  ## ... if not provide gamUtils' version
  fnam <- paste0(".qf.", fam$family)
  
  fam$qf <- get(fnam, mode = "function", envir = asNamespace("gamUtils"))
  
  fam  
}
