#' Generate fitted values from a estimated GAM (censored log normal version)
#'
#' This is a stand-in replacement for `gratia::fitted_values` to be used with
#' the clognorm family and make predictions on the response scale.
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
#' @export
#' @rdname fitted_values
`fitted_values_clognorm` <- function(object,
                                data = NULL,
                                scale = c(
                                  "response",
                                  "link",
                                  "linear predictor"
                                ),
                                ci_level = 0.95, ...) {
  if (is.null(data)) {
    data <- gratia:::delete_response(object, model_frame = FALSE) |>
      as_tibble()
  } else if (!is_tibble(data)) {
    data <- as_tibble(data)
  }

  # get the transformation used
  base <- attr(object$family, "base")
  base <- if(base=="e") exp(1) else base
  trans <- if(base=="e") exp else function(x) base^x

  fit <- predict(object,
    newdata = data, ..., type = "response",
    se.fit = TRUE
  ) |>
    as.data.frame() |>
    rlang::set_names(c(".fitted", ".se")) |>
    as_tibble()

  fit <- fit |>
    mutate(.fitted = logb(.fitted, base=base))

  # add .row *unless* it already exists
  if (!".row" %in% names(data)) {
    fit <- mutate(fit, .row = row_number())
  }
  fit <- bind_cols(data, fit) |>
    relocate(".row", .before = 1L)

  # create the confidence interval
  crit <- gratia:::coverage_normal(ci_level)
  fit <- mutate(fit,
    ".lower_ci" = .data[[".fitted"]] - (crit * .data[[".se"]]),
    ".upper_ci" = .data[[".fitted"]] + (crit * .data[[".se"]])
  )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = trans
      ))
  }

  fit
}
