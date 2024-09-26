#' Observed versus expected diagnostic
#'
#' Given a covariate, calculate the observed and expected counts for each
#' unique value of the covariate. This can be a useful goodness of fit check
#' for GAMs when modelling count data. It might make sense in other cases.
#'
#' One strategy for model checking is to calculate observed and expected counts
#' at different aggregations of the variable. If these match well then the
#' model fit is good.
#'
#' @param model a fitted GAM
#' @param covar covariate to aggregate by (`character`)
#' @param cut vector of cut points to aggregate at. If not supplied, the unique
#' values of `covar` are used.
#' @export
#' @author David L Miller, on the suggestion of Mark Bravington.
#' @export
#' @return `data.frame` with values of observed and expected counts.
#' @examples
#' # simulate poisson data...
#' set.seed(6)
#' dat <- gamSim(1,n=200,dist="poisson",scale=.1)
#' 
#' b2<-gam(y~s(x0)+s(x1)+s(x2)+
#'         s(x3),family=poisson,data=dat,method="REML", control=gam.control(keepData=TRUE))
#' plot(b2,pages=1)

obs_exp <- function(model, covar, cut=NULL){

  # get data -- does this always work?
  oe <- get(model$call$data)
  # add in predictions
  oe$predicted <- predict(model)

  # do the aggregation if necessary
  if(!is.null(cut)){
    oe[[covar]] <- cut(oe[[covar]], breaks=cut)
  }

  # for each unique value of term, sum observed and expected
  oe <- plyr::ddply(oe, covar, function(x){
    data.frame(Observed = sum(x$y),
               Expected = sum(x$predicted))
  })

  # format the table
  cn <- oe[,1]
  oe <- t(oe[,2:3])
  colnames(oe) <- cn

  return(oe)
}
