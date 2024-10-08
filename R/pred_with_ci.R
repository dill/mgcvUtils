#' Make a prediction and put a confidence interval around it
#'
#' Small utility function to append a `data.frame` with predictions from a GAM
#' and include a confidence interval. Various things are customizable.
#'
#' By the `data` argument (a `data.frame`) will be returned with 3 extra
#' columns, one for the prediction (column name as in `pred_label`), then upper
#' CI (named `paste(ci_label, ci_types[1], sep="_")`) and finally the
#' lower CI (named `paste(ci_label, ci_types[2], sep="_")`). If `label`
#' and `label_label` are non-empty, a column will be added (with name
#' `label_label`) and contents as in `label`.
#'
#' @param model a fitted GAM
#' @param data the data we wish to predict to
#' @param alpha the level we want our confidence intervals at
#' @param label label for these models (e.g., "model_1")
#' @param label_label the column name for the model labels
#' @param pred_label the column name for the predictions
#' @param ci_label start of the column name for the confidence intervals
#' @param ci_types a 2-vector with the suffixes for the confidence intervals
#' (upper then lower)
#' @param sep separator between `ci_label` and `ci_types` in the confidence
#' intervals column name
#' @author David L Miller
#' @export
#' @importFrom mgcv predict
#' @examples
#' library(mgcvUtils)
#'
#' # fit something to look at
#' set.seed(2)
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(y~s(x0),data=dat)
#'
#' # adding prediction/ci columns
#' dat2 <- pred_with_ci(b, dat)
#' head(dat2)
#'
#' # add labels, so you can concatenate predictions later for
#' # plotting with ggplot2
#' dat3 <- pred_with_ci(b, dat, label="model1", label_label="model_id")
#' head(dat3)
pred_with_ci <- function(model, data, alpha=0.05, label=NULL, label_label=NULL,
                         pred_label="pred", ci_label="ci",
                         ci_types=c("u", "l"), sep="_"){

  crit <- qnorm(1-alpha)

  u_ci_lab <- paste(ci_label, ci_types[1], sep=sep)
  l_ci_lab <- paste(ci_label, ci_types[2], sep=sep)

  # label labels :)
  if(!is.null(label) & !is.null(label_label)){
    data[[label_label]] <- label
  }

  temp <- predict(model, se=TRUE)
  data[[pred_label]] <- temp$fit
  data[[u_ci_lab]] <- temp$fit + crit*temp$se.fit
  data[[l_ci_lab]] <- temp$fit - crit*temp$se.fit


  data
}
