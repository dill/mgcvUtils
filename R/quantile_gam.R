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
quantile.gam <- function(x, probs, newdata, w = NULL, offset = NULL, ...){
  
  o <- x
  
  sig <- o$sig2
  if ( is.null(sig) ){ sig <- summary(o)$dispersion }
  
  # Either (a) use data in GAM object or (b) predict using new data 
  if( missing(newdata) ){ # (a) the offset should already be included in o$fitted.values
    
    mu <- o$fitted.values
    if( is.null(w) ) { w <- o$prior.weights }
    if( !is.null(offset) ) { message("simulate.gam: offset argument ignored. No newdata provided, so offset is already in object$fitted.values")}
    
  } else{ # (b) the user-defined offset is added to linear predictor
    
    mu <- predict(o, newdata = newdata, type = "link")
    if( is.null(w) ){ w <- mu*0 + 1 }
    
    # Dealing with offset and inverting link function
    form <- o$formula
    if( is.list(form) ){ # [1] GAMLSS case
      n <- length( mu[[1]] )
      nte <- length( form )
      lnki <- lapply(o$family$linfo, "[[", "linkinv")
      if( is.null(offset) ) { offset <- rlply(nte, { numeric(n) }) }
      mu <- t( laply(1:nte, function(.ii){ lnki[[.ii]](mu[ , .ii] + offset[[.ii]]) }) )
    } else { # [2] GAM case
      if( is.null(offset) ) { offset <- mu * 0 }
      mu <- o$family$linkinv( mu + offset )
    }
    
  }
  
  o$family <- fix_family_qf( o$family )

  out <- sapply(probs, function(.p){
    tmp <- o$family$qf(p = .p, mu = mu, wt = w, scale = sig)
    return( tmp )
  })
  
  if( is.matrix(out) ){ colnames( out ) <- probs }
  
  return( out )
  
}