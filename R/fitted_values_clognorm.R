#' Generate fitted values from a estimated GAM (censored log normal version)
#'
#' This is a stand-in replacement for `gratia::fitted_values` to be used with
#' the `clognorm` family and make predictions on the response scale.
#'
#' Note that as of `gratia` 0.11.2-3 this function is not necessary. This
#' function is retained here until there is a `gratia` CRAN release.
#'
#' @param object a fitted model using `clognorm` as the response.
#' @param data optional data frame of covariate values for which fitted values
#'   are to be returned.
#' @param scale character; what scale should the fitted values be returned on?
#'   use `"response"` to obtain intervals on the response scale (exponentiated)
#'   for a log-normally distributed response.
#' @param ci_level numeric; a value between 0 and 1 indicating the coverage of
#'   the credible interval.
#' @param ... arguments passed to [mgcv::predict.gam()]. Note that `type`,
#'   `newdata`, and `se.fit` are already used and passed on to
#'   [mgcv::predict.gam()].
#'
#' @return A tibble (data frame) whose first *m* columns contain either the data
#'   used to fit the model (if `data` was `NULL`), or the variables supplied to
#'  `data`. Four further columns are added:
#'
#' * `fitted`: the fitted values on the specified scale,
#' * `se`: the standard error of the fitted values (always on the *link* scale),
#' * `lower`, `upper`: the limits of the credible interval on the fitted values,
#'     on the specified scale.
#'
#' @seealso clognorm
#' @importFrom tibble as_tibble is_tibble
#' @importFrom utils getFromNamespace
#' @export
#' @rdname fitted_values_clognorm
`fitted_values_clognorm` <- function(object,
                                data = NULL,
                                scale = c(
                                  "response",
                                  "link",
                                  "linear predictor"
                                ),
                                ci_level = 0.95, ...) {

  if(!grepl("clog.+norm(.+)", object$family$family)){
    stop("Only useful when using a clognorm family.")
  }

  # import unexported functions cheat code
  delete_response <- utils::getFromNamespace("delete_response", "gratia")
  coverage_normal <- utils::getFromNamespace("coverage_normal", "gratia")

  if (is.null(data)) {
    data <- delete_response(object, model_frame = FALSE) |>
      as_tibble()
  } else if (!is_tibble(data)) {
    data <- as_tibble(data)
  }

  # get the transformation used
  basec <- attr(object$family, "base")
  base <- if(basec=="e") exp(1) else basec
  trans <- if(basec=="e") exp else function(x) base^x

  fit <- predict(object,
    newdata = data, ..., type = "response",
    se.fit = TRUE
  ) |>
    as.data.frame()

  names(fit) <- c(".fitted", ".se")
  fit <- as_tibble(fit)

  fit$.fitted <- logb(fit[[".fitted"]], base=base)

  fit <- cbind(data, fit)

  # add .row *unless* it already exists
  if (!".row" %in% names(fit)) {
    fit <- cbind(.row=1:nrow(fit), fit)
  }

  # create the confidence interval
  crit <- coverage_normal(ci_level)
  fit[[".lower_ci"]] <- fit[[".fitted"]] - (crit * fit[[".se"]])
  fit[[".upper_ci"]] <- fit[[".fitted"]] + (crit * fit[[".se"]])

  scale <- match.arg(scale)

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    fit[[".lower_ci"]] <- trans(fit[[".lower_ci"]])
    fit[[".upper_ci"]] <- trans(fit[[".upper_ci"]])
    fit[[".fitted"]] <- trans(fit[[".fitted"]])
  }

  fit
}
