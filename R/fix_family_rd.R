#' Fix $rd() slot of a mgcv family
#' 
#' @description Fix $rd() slot of a mgcv family.
#' 
#' @param fam an \code{mgcv} family
#' 
#' @examples
#' library(mgcvUtils)
#' set.seed(525)
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(list(y~s(x0),~s(x1),~1),data=dat,family=gevlss)
#' 
#' head( simulate(b, nsim = 2) )
#' 
#' @importFrom mgcv fix.family.rd
#' @export
#'
fix_family_rd <- function(fam) {
  
  fam <- mgcv::fix.family.rd(fam)
  
  ## Try if mgcv provides $rd slot...
  if( !is.null(fam$rd) ) {
    return( fam )
  }
  
  ## ... if not provide mgcvUtils' version
  fnam <- paste0(".rd.", fam$family)
  
  fam$rd <- get(fnam, mode = "function", envir = asNamespace("mgcvUtils"))
  
  fam  
}
