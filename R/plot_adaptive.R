#' Plot univariate adaptive smoother penalties
#'
#' If we use an adaptive smoother, we can plot $\lambda(x)$ over $x$ to give an
#' idea of how the smoothing parameter changes as a function of the covariate.
#'
#' @param model a fitted `mgcv` model
#' @param term which term number in the model we want to plot
#' @param bs the basis used for the smoothing parameter smoother
#' @param m the order of the smoothing parameter smoother
#' @param nplot number of points used to make the grid for the plot
#' @return just makes a plot
#' @export
#' @importFrom mgcv smooth.construct PredictMat
#' @export
#' @author David L Miller with idea from Philip Dixon
#' @examples
#' library(mgcv)
#' library(MASS)
#'
#' # fit an adaptive smooth to the mcycle data
#' b_ad <- gam(accel ~ s(times, bs="ad"), data=mcycle)
#'
#' par(mfrow=c(1,2))
#' plot(b_ad)
#' plot_adaptive_1d(b_ad)
plot_adaptive_1d <- function(model, term=1, bs="ps", m=2, nplot=100){

  # this is the number of parameters for the ad smooth
  # (as opposed to the penalty)
  nk <- model$smooth[[term]]$bs.dim

  # spline basis size is number of smoothing parameters
  # this is gamma
  k <- length(model$sp)

  # this is the basis setup as happens within
  # smooth.construct.ad.smooth.spec

  # create a grid with points equally spaced over 0,1
  # (as many as there are basis functions in the main smooth)
  x <- 1:(nk-2)/nk

  # construct the smoother object
  s2 <- smooth.construct(s(x, k=k, bs="ps", m=m, fx=TRUE),
                         data=data.frame(x=x),
                         knots=NULL)

  # we could just extract the $X element from the above, but it's
  # nicer to have something at an arbitrary resolution
  pp <- PredictMat(s2, data.frame(x=seq(0, 1, length.out=nplot)))

  # first and last "internal" knots
  ktsmm <- model$smooth[[1]]$knots
  ktsmm <- ktsmm[c(1+m/2, length(ktsmm)-1-m/2)]

  # get predicted values and the prediction grid
  gr <- data.frame(pred = pp%*%model$sp,
                   x    = seq(ktsmm[1], ktsmm[2], length.out=nplot))

  plot(gr$x, gr$pred, type="l",
       xlab=model$smooth[[term]]$term, ylab="Smoothing parameter")
}
