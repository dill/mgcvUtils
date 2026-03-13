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
#' @return just makes a plot, invisably returns the data underlying said plot
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

  invisible(gr)
}

#' Plot bivariate adaptive smoother penalties
#'
#' If we use an adaptive smoother, we can plot $\lambda(x,y)$ over $x,y$ to
#' give an idea of how the smoothing parameter changes as a function of the
#' covariate.
#'
#' This function does not cover all corner cases, your mileage may vary!
#'
#' @param model a fitted `mgcv` model
#' @param term which term number in the model we want to plot
#' @param bs the basis used for the smoothing parameter smoother
#' @param m the order of the smoothing parameter smoother
#' @param nplot2 number of points used to make the grid for the plot
#' @return just makes a plot and invisibly returns the data used to make said
#' plot
#' @export
#' @importFrom mgcv smooth.construct PredictMat
#' @export
#' @author David L Miller with idea from Philip Dixon
#' @examples
#' # 2D example from ?smooth.construct.ad.smooth.spec
#'
#' par(mfrow=c(2,2))
#' x <- seq(-.5, 1.5, length= 60)
#' z <- x
#' f3 <- function(x,z,k=15) { r<-sqrt(x^2+z^2);f<-exp(-r^2*k);f}
#' f <- outer(x, z, f3)
#' op <- par(bg = "white")
#'
#' ## Plot truth....
#' persp(x,z,f,theta=30,phi=30,col="lightblue",ticktype="detailed")
#'
#' n <- 2000
#' x <- runif(n)*2-.5
#' z <- runif(n)*2-.5
#' f <- f3(x,z)
#' y <- f + rnorm(n)*.1
#'
#' ## Try tprs for comparison...
#' b0 <- gam(y~s(x, z, k=150))
#' vis.gam(b0, c("x", "z"), plot.type="contour")
#'
#' ## Now adaptive...
#' b <- gam(y~s(x, z, bs="ad", k=15, m=5), gamma=1.4)
#' vis.gam(b, c("x", "z"), plot.type="contour")
#'
#' plot_adaptive_2d(b)
plot_adaptive_2d <- function(model, term=1, bs="ps", m=2, nplot2=100){

  # extract penalty smooth object
  ps <- model$smooth[[term]]$pen.smooth
  # main smooth object
  ms <- model$smooth[[term]]

  # get the range of the data in each direction (for the plot labels)
  # note we need the m+2 to k+1 range where the spline is defined
  # see Wood 2017 sec 5.3.3
  ktsi <- ms$margin[[1]]$knots[(ms$margin[[1]]$m[1]+2):(ms$margin[[1]]$bs.dim+1)]
  ktsj <- ms$margin[[2]]$knots[(ms$margin[[2]]$m[1]+2):(ms$margin[[2]]$bs.dim+1)]

  # make a plotting grid
  ii <- seq(0, 1, length.out=nplot2)
  jj <- seq(0, 1, length.out=nplot2)
  gr <- expand.grid(i=ii, j=jj)
  # generate plot axis ticks
  ktsii <- seq(min(ktsi), max(ktsi), length.out=nplot2)
  ktsjj <- seq(min(ktsj), max(ktsj), length.out=nplot2)

  # create prediction matrix
  pp <- PredictMat(ps, gr)

  # predict
  gr$pred <- pp%*%b$sp

  # plot
  image(ktsii, ktsjj, matrix(gr$pred, nplot2, nplot2),
        xlab=ms$margin[[1]]$term, ylab=ms$margin[[2]]$term)

  invisible(gr)
}
