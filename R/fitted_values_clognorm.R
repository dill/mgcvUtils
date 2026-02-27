#' Generate fitted values from a estimated GAM
#'
#' @param object a fitted model. Currently only models fitted by [mgcv::gam()]
#'   and [mgcv::bam()] are supported.
#' @param data optional data frame of covariate values for which fitted values
#'   are to be returned.
#' @param scale character; what scale should the fitted values be returned on?
#'   `"linear predictor"` is a synonym for `"link"` if you prefer that
#'   terminology.
#' @param ci_level numeric; a value between 0 and 1 indicating the coverage of
#'   the credible interval.
#' @param ... arguments passed to [mgcv::predict.gam()]. Note that `type`,
#'   `newdata`, and `se.fit` are already used and passed on to
#'   [mgcv::predict.gam()].
#'
#' @note For most families, regardless of the scale on which the fitted values
#'   are returned, the `se` component of the returned object is on the *link*
#'   (*linear predictor*) scale, not the response scale. An exception is the
#'   `mgcv::ocat()` family, for which the `se` is on the response scale if
#'   `scale = "response"`.
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
#' Models fitted with certain families will include additional variables
#'
#' * `mgcv::ocat()` models: when `scale = "repsonse"`, the returned object will
#'   contain a `row` column and a `category` column, which indicate to which row
#'   of the `data` each row of the returned object belongs. Additionally, there
#'   will be `nrow(data) * n_categories` rows in the returned object; each row
#'   is the predicted probability for a single category of the response.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 6)
#' }
#' sim_df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = sim_df, method = "REML")
#' fv <- fitted_values(m)
#' fv
#' \dontshow{
#' options(op)
#' }
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
