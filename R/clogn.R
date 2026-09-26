# censored log-normal distribution helpers

# This work was funded by the Department of Environment, Food and Rural
# Affairs, Natural Capital Ecosystem Assessment Programme. It was managed by
# the Environment Agency and delivered by the UK Centre for Ecology and
# Hydrology, under Research, Development and Evidence Framework contract
# RDE945.

# AI declaration: Derivations were checked using Claude Opus 5 (then checked
# again by a human).

#' Censored log-normal distribution for `mgcv`
#'
#' Implementation of a censored log-normal distribution. This uses `mgcv`'s
#' built-in `cnorm` distribution for model fitting. Supply the response and
#' all finite censoring bounds on the logarithmic scale, and specify the
#' matching `base`. The family does not log-transform the supplied response.
#' Preserve the `-Inf` and `Inf` censoring indicators. A
#' special `predict` function to provide unbiased predicted values using the
#' arithmetic mean and to account for bias incurred from random effects.
#' post-hoc transformations to give "correct" results. See [[mgcv::cnorm]] for
#' more information on how to specify censoring.
#'
#' Fitting weights are precision weights, as in [mgcv::cnorm()]. They are
#' included in the posterior correction. Response predictions describe the
#' arithmetic mean at unit precision (log-scale residual variance
#' `sigma^2`), including when predicting at the training covariates.
#'
#' Further mathematical details are given in the `inst/maths/` directory of
#' this package.
#'
#' @param base the base of the logarithm used. base may be any number or "e"
#' @param theta kept as a placeholder, but only estimated `theta` is supported.
#' @param link The link function: only '"identity"' is supported
#' @return `family` object
#' @importFrom mgcv predict.gam predict.bam
#' @importFrom stats dnorm
#' @export
#' @author David L Miller with code modified from mgcv by Simon Wood
#' @examples
#' library(mgcv)
#' library(mgcvUtils)
#' library(ggplot2)
#'
#' # make a 1D example
#' set.seed(123)
#'
#' # simulated example from gam
#' data <- gamSim(1, n = 400, dist = "normal", scale = 1)
#'
#' noise <- rnorm(nrow(data), sd = 0.1)
#' data$y2 <- 10^(data$f2 / 10 + noise)
#'
#' # transform the response before adding censoring indicators
#' data$y2 <- log10(data$y2)
#' data$y2 <- cbind(data$y2, data$y2)
#'
#' # indicate which values are censored, see ?cnorm for rules
#' # censor below 2 on the original scale, using the same log base
#' cen_level <- log10(2)
#' data$y2[data$y2[, 1] < cen_level, 2] <- -Inf
#' data$y2[data$y2[, 1] < cen_level, 1] <- cen_level
#'
#' b2 <- gam(y2~s(x2), data=data, method="REML", family=clognorm(base=10))
#' summary(b2)
#'
#' # compare plotted predictions on the response scale
#' # create prediction data
#' pred <- data.frame(x2=seq(min(data$x2), max(data$x2), length.out=300))
#'
#' # uncorrected (on link scale)
#' old <- predict(b2, newdata=pred, se=TRUE)
#' # corrected (must use type="response")
#' new <- predict(b2, newdata=pred, type="response", se=TRUE)
#'
#' # make data for the plot
#' predict_df <- data.frame(
#'   x = rep(pred$x2, 2),
#'   pred = c(10^old$fit, new$fit),
#'   ci_lower = c(10^(old$fit - 1.96*old$se.fit),
#'                new$fit - 1.96*new$se.fit),
#'   ci_upper = c(10^(old$fit + 1.96*old$se.fit),
#'                new$fit + 1.96*new$se.fit),
#'   model = rep(c("cnorm", "clognorm"), each=nrow(pred)))
#'
#' # create plot
#' p <- ggplot(predict_df, aes(x=x)) +
#'   geom_point(aes(x=x2, y=10^y2[,1]), data=data) +
#'   geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper, fill=model),
#'               alpha=0.2) +
#'   geom_line(aes(y=pred, colour=model)) +
#'   scale_y_continuous(trans="log10") +
#'   labs(x="x2", y="Predictions on response scale") +
#'   theme_bw()
#' print(p)
clognorm <- function (theta = NULL, link = "identity", base=10) {

  # borrow find dull deviance from mgcv cheat code
  find.null.dev <- utils::getFromNamespace("find.null.dev", "mgcv")

  if(!is.null(theta) || link != "identity"){
    stop("Only estimated theta and identity link are supported")
  }

  # first make a copy of mgcv::cnorm by Simon Wood
  cln <- mgcv::cnorm()#theta=theta, link=link)

  # get the distribution name right
  cln$name <- "clognorm"

  # save the base used for later
  attr(cln, "base") <- base

  # Share prediction metadata between callbacks. mgcv only copies selected
  # postproc results, so attributes on its return value are not carried over.
  # Like cnorm's theta environment, this state belongs to this family instance.
  state <- new.env(parent = emptyenv())
  state$Xc <- NULL
  state$tw <- state$qw <- numeric(0)

  cln$postproc <- function(family, y, prior.weights, fitted, linear.predictors,
                           offset, intercept){

    # truly cursed code
    G <- get("G", envir = parent.frame())
    XX <- G$X
    rm(G)

    posr <- list()

    if (is.matrix(y)) {
      .yat <- y[,2]
      y <- y[,1]
      attr(y,"censor") <- .yat
    } else {
      .yat <- attr(y, "censor")
      if (is.null(.yat)) .yat <- y
    }

    posr$null.deviance <- find.null.dev(family, y,
                                        eta=linear.predictors, offset,
                                        prior.weights)
    posr$family <- paste("clog", attr(family, "base"),
                         "norm(",round(family$getTheta(TRUE),3),")",sep="")

    # extra info needed for prediction later on when doing various
    # corrections
    cind <- .yat != y
    state$Xc <- XX[cind, , drop = FALSE]
    # cnorm's deviance derivatives cover both one-sided censoring directions,
    # interval bounds and observation-specific precision weights. Deviance
    # is twice the negative log likelihood, up to beta-independent terms.
    state$tw <- state$qw <- numeric(0)
    if (any(cind)) {
      derivs <- family$Dd(y, linear.predictors, family$getTheta(),
                          prior.weights, level = 2)
      state$tw <- derivs$Dmu3[cind] / 2
      state$qw <- derivs$Dmu4[cind] / 2
    }

    posr
  } ## postproc

  # prediction method
  # note that this is called with predict(..., type="response") ONLY
  cln$predict <- function(family, se=FALSE, eta=NULL, y=NULL, X=NULL,
                          beta=NULL, off=NULL, Vb=NULL) {

    # previous version had to form tensors via kronecker products, now:
    # All contractions of D^3 h, D^4 h and D^3 g collapse to scalar sums because
    # g and the likelihood depend on beta only through linear predictors. No
    # 3- or 4-tensor is ever formed; the largest object is p x p.

    # Only censored observations contribute third and fourth derivatives:
    # uncensored likelihood contributions and penalties are quadratic in beta.
    Xc <- state$Xc
    tw <- state$tw
    qw <- state$qw
    # scale parameter
    sigma <- family$getTheta(TRUE)

    # get the transformation used
    basee <- attr(family, "base")
    base <- if(basee=="e") exp(1) else base

    cc <- log(base)
    sh <- 0.5 * sigma^2 * cc
    nc <- if(is.null(Xc)) 0 else nrow(Xc)

    # all the nc>0 parts are checking if there is censoring
    # those contributions only matter when there are censored observations
    if (nc > 0){
      # x_i' V x_i, shared across rows
      d <- rowSums((Xc %*% Vb) * Xc)
    } else {
      tw <- qw <- d <- numeric(0)
    }

    eta0 <- drop(X %*% beta)
    # Formula offsets are supplied separately from the prediction matrix.
    if (!is.null(off)) eta0 <- eta0 + off
    fit <- rep(NA, nrow(X))
    # need to return 0 if not calculated
    se.fit <- rep(0, nrow(X))

    # iterate over prediction data
    for (i in seq_len(nrow(X))){
      x0  <- X[i, ]
      g0  <- base^(eta0[i] + sh)
      a   <- drop(Vb %*% x0)
      # x0' V x0
      q00 <- drop(x0 %*% a)
      e   <- if (nc > 0L) drop(Xc %*% a) else numeric(0)

      # expectation
      # 1/2 tr(V D2g)
      Ecorr <- 0.5 * cc^2 * q00
      # 1/2 T[V, V Dg]
      Eskew <- if (nc > 0L) 0.5 * cc * sum(tw * d * e) else 0
      # whole thing
      fit[i] <- g0 * (1 + Ecorr - Eskew)

      if(se){
        # Variance divided by g0^2: factor out the response mean before
        # squaring to avoid overflow (or underflow) in intermediate terms.
        # B = c x0 x0' - sum_i t_i e_i x_i x_i'
        # so that V(D2g - T[VDg]) = c g0 V B
        B  <- cc * tcrossprod(x0)
        if(nc > 0) B <- B - crossprod(Xc, Xc * (tw * e))

        VB <- Vb %*% B
        # (Dg)' V (Dg)
        v1 <- cc^2 * q00
        # 1/2 tr([V(D2g - T[VDg])]^2)
        v2 <- 0.5 * cc^2 * sum(VB * t(VB))
        # tr(V D3g[VDg])
        v3 <- cc^4 * q00^2
        v4 <- v5 <- 0

        if(nc > 0L) {
          # -1/2 tr(V Q[VDg,VDg])
          v4 <- -0.5 * cc^2 * sum(qw * e^2 * d)
          # beta'' divided by g0^2
          bpp <- 2 * cc^3 * q00 * a -
                 cc^2 * drop(Vb %*% crossprod(Xc, tw * e^2))
          # -1/2 tr(V T[beta''])
          v5 <- -0.5 * sum(tw * drop(Xc %*% bpp) * d)
        }
        # Restore the response scale after taking the square root.
        se.fit[i] <- abs(g0) * sqrt(v1 + v2 + v3 + v4 + v5)
      } # end se calculations
    } # end iteration over predictions

    # must return list with elements fit and se.fit!
    list(fit=fit, se.fit=se.fit)
  } # end predict

  cln
}
