#' Conditional quantiles a GAM object
#' 
#' @description This method can be used to get the estimated conditional quantiles of the response 
#'              variable. 
#' 
#' @param x the output of a \code{gam()} or \code{bam()} call.
#' @param probs numeric vector of probabilities with values in (0,1).
#' @param newdata Optional new data frame or list to be passed to \link{predict.gam}.
#' @param ... funther arguments to be passed to \code{predict}.
#' @return A matrix where each column is a vector of estimated quantiles responses. 
#'         The number of rows is equal to the number of responses in the fitted object or in \code{newdata}.
#' @examples
#' library(gamUtils); library(MASS)
#' b <- gam(list(accel~s(times,k=20,bs="ad"),~s(times)),
#'          data=mcycle,family=gaulss())
#' 
#' b$family <- fix_family_qf( b$family )
#' qfit <- quantile(b, c(0.05, 0.25, 0.5, 0.75, 0.95))
#' 
#' plot(mcycle, ylim = c(-160, 100))
#' for(ii in 1:ncol(qfit)){
#'   lines(mcycle$times, qfit[ , ii], col = 2)
#' }
#' @importFrom stats quantile
#' @export quantile.gam
#' @export
#'
quantile.gam <- function(x, probs, newdata = NULL, ...){
  
  x$family <- fix_family_qf( x$family )
  
  if( is.null(newdata) ){
    mu <- x$fitted.values
  } else {
    mu <- predict(x, type = "response", newdata = newdata, ...)
  }
  
  # NEED TO DEAL PROPERLY WITH WT AND SCALE AT SOME POINT!
  out <- sapply(probs, function(.p){
    .o <- x$family$qf(p = .p, mu = mu, wt = NULL, scale = NULL)
    return( .o )
  })
  
  colnames( out ) <- probs 
  
  return( out )
  
}