# censored log-normal distribution helpers

# This work was funded by the Department of Environment, Food and Rural
# Affairs, Natural Capital Ecosystem Assessment Programme. It was managed by
# the Environment Agency and delivered by the UK Centre for Ecology and
# Hydrology, under Research, Development and Evidence Framework contract
# RDE945.

#' Censored log-normal distribution for `mgcv`
#'
#'
#' @param theta log standard deviation parameter. If supplied and positive then
#' taken as a fixed value of standard deviation (not its log). If supplied and
#' negative taken as negative of initial value for standard deviation (not its
#' log).
#' @param link The link function: only '"identity"' is supported
#' @param base the base of the logarithm used. base may be any number or "e"
#' @return `family` object
#' @importFrom mgcv predict.gam predict.bam
#' @export
#' @author David L Miller with code modified from mgcv by Simon Wood
#' @examples
#' library(mgcv)
#' library(mgcvUtils)
#' library(ggplot2)
#'
#' # simulated example from ?gam
#' dat <- gamSim(1,n=2000,dist="normal",scale=1)
#'
#' # make a 1D example with less noise
#' set.seed(2)
#'
#' e <- rnorm(nrow(dat), sd=2)
#' dat$y2 <- exp(dat$f2/10 + e)
#'
#' # censor values below some value
#' dat$y2 <- cbind(dat$y2, dat$y2)
#' dat$y2[dat$y2[, 1] < 0.01, 2] <- -Inf
#' dat$y2[dat$y2[, 1] < 0.01, 1] <- 1
#'
#' b2 <- gam(log(y2)~s(x2),data=dat, method="REML", family=clognorm(base=10))
#' summary(b2)
#'
#' # compare predictions (on a random subset of the data)
#' pred <- dat[sample(1:nrow(dat), 100), ]
#' pred$y2 <- NULL
#' old <- (predict(b2, newdata=pred, se=TRUE))
#' # corrected
#' new <- predict(b2, newdata=pred, type="response", se=TRUE)
#'
#' preddat <- data.frame(pred.old = 10^old$fit,
#'                       pred.cor = new$fit,
#'                       pred.upper.old = 10^(old$fit + 1.96*old$se.fit),
#'                       pred.upper.cor = 10^(log10(new$fit) + 1.96*new$se.fit),
#'                       pred.lower.old = 10^(old$fit - 1.96*old$se.fit),
#'                       pred.lower.cor = 10^(log10(new$fit) - 1.96*new$se.fit))
#'
#' ggplot(preddat) +
#'   geom_point(aes(x=pred.old, y=pred.cor)) +
#'   geom_segment(aes(x=pred.lower.old, xend=pred.upper.old, y=pred.cor, yend=pred.cor)) +
#'   geom_segment(aes(y=pred.lower.cor, yend=pred.upper.cor, x=pred.old, xend=pred.old)) +
#'   theme_minimal()
clognorm <- function (theta = NULL, link = "identity", base=10) {

  # borrow find dull deviance from mgcv cheat code
  find.null.dev <- utils::getFromNamespace("find.null.dev", "mgcv")

  if(!is.null(theta) || link != "identity"){
    stop("Only estimated theta and identity link are supported")
  }

  # first make a copy of mgcv::cnorm by Simon Wood
  cln <- mgcv::cnorm()#theta=theta, link=link)

  cln$name <- "clognorm"

  attr(cln, "base") <- base

  cln$postproc <- function(family, y, prior.weights, fitted, linear.predictors,
                       offset, intercept){
    posr <- list()
    if (is.matrix(y)) {
      .yat <- y[,2]
      y <- y[,1]
      attr(y,"censor") <- .yat
    }
    posr$null.deviance <- find.null.dev(family, y,
                                        eta=linear.predictors, offset,
                                        prior.weights)
    posr$family <- paste("clog", attr(family, "base"),
                         "norm(",round(family$getTheta(TRUE),3),")",sep="")
    posr
  } ## postproc

  # prediction method
  # note that this is called with predict(..., type="response") ONLY
  cln$predict <- function(family, se=FALSE, eta=NULL, y=NULL, X=NULL,
                          beta=NULL, off=NULL, Vb=NULL) {


    # get the transformation used
    base <- attr(family, "base")
    base <- if(base=="e") exp(1) else base
    trans <- if(base=="e") exp else function(x) base^x

    phi <- function(beta, Xp) trans(Xp%*%beta)

    n <- nrow(X)
    Vb2 <- Vb %*% Vb
    Vb3 <- Vb2 %*% Vb

    # linear predictor
    pred <- X%*%beta
    tpred <- trans(pred)

    # this matrix is the same for each derivative so just calculate once
    basemat <- matrix(log(base)^2, length(beta), length(beta))

    # constants
    D3gb <- log(base)^3
    n1 <- 1/(2*n)
    n12 <- n1*n1

    # correction vector storage
    corr1 <- var.corr <- pred*0

    # derivatives of g (exp) with respect to parameters for all
    # observations, calculate once
    # sweep deals with wanting to multiply each row of design matrix
    # by its prediction without duplicating tpred and using *
    if(se){
      Dg <- log(base) * sweep(X, 1, tpred, "*")
      vv <- diag(tcrossprod(Dg%*%Vb, Dg))/n
    }

    # loop over observations calculating the correction for prediction
    # (and se if requested)
    for(i in 1:nrow(X)){

      # calculate correction
      hess <- tcrossprod(X[i, ], X[i, ]) * basemat * tpred[i]
      corr1[i] <- n1 * sum(diag( Vb%*%hess))

      # this is probably not very efficient!
      # extra bits needed for standard error calculation
      if(se){
        hess2 <- crossprod(hess, hess)
        D3g <- kronecker(hess, t(X[i, , drop=FALSE]))
        se.part <- kronecker(diag(length(beta)), Dg[i, ]) %*% Vb2
        cpHVb <- crossprod(hess, Vb)

        var.corr[i] <- n12 * sum(diag(D3gb*crossprod(D3g, se.part) -
                                      crossprod(cpHVb, cpHVb)))
      }
    }

    # return the se if we calculated it
    if(se){
      se <- sqrt(vv - var.corr)
    }else{
      se <- pred*0
    }

    # must return list with elements fit and se.fit!
    list(fit=as.vector(tpred + corr1), se.fit=se)
  }

  cln
}
