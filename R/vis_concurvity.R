#' Visualise concurvity between terms in a GAM
#'
#' Plot measures of how much one term in the model could be explained by
#' another. When values are high, one should consider re-running variable
#' selection with one of the offending variables removed to check for stability
#' in term selection.
#'
#' These methods are considered somewhat experimental at this time. Consult
#' [`concurvity`][mgcv::concurvity] for more information on how concurvity
#' measures are calculated.
#'
#' @param model fitted model
#' @param type concurvity measure to plot, see [`concurvity`][mgcv::concurvity]
#' @author David L Miller
#' @export
#' @importFrom graphics image layout
#' @examples
#' # example from ?concurvity
#' library(mgcv)
#' ## simulate data with concurvity...
#' set.seed(8);n<- 200
#' f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
#'             (10 * x)^3 * (1 - x)^10
#' t <- sort(runif(n)) ## first covariate
#' ## make covariate x a smooth function of t + noise...
#' x <- f2(t) + rnorm(n)*3
#' ## simulate response dependent on t and x...
#' y <- sin(4*pi*t) + exp(x/20) + rnorm(n)*.3
#'
#' ## fit model...
#' b <- gam(y ~ s(t,k=15) + s(x,k=15),method="REML")
#'
#' ## assess concurvity between each term and `rest of model'...
#' vis_concurvity(b)
#'
#' ## ... and now look at pairwise concurvity between terms...
#' vis_concurvity(b,full=FALSE)
vis_concurvity <- function(model, type="estimate"){

  # calculate concurvity for this model
  cc <- concurvity(model, full=FALSE)[[type]]

  # remove diagonal elements
  diag(cc) <- NA

  # setup plotting
  layout(matrix(1:2, ncol=2), widths=c(5,1))
  opar <- par(mar=c(5, 6, 5, 0) + 0.1)

  # main plot
  image(z=cc, x=seq_len(ncol(cc)), y=seq_len(nrow(cc)), ylab="", xlab="",
        axes=FALSE, asp=1, zlim=c(0,1))
  axis(1, at=seq_len(ncol(cc)), labels = colnames(cc), las=2, lwd=0)
  axis(2, at=seq_len(nrow(cc)), labels = rownames(cc), las=2, lwd=0)

  # legend
  opar <- par(mar=c(5, 0, 4, 3) + 0.1)
  image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),
        x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="")
  axis(4, at=seq(1, 101, len=5), labels = round(seq(0, 1, len=5), 1), las=2)

  # reset graphical pars
  par(opar)
}
