# Implementation overview
#
# The model is a common smooth surface plus deviations for the levels of f.
# This file builds the deviations. At each covariate combination, they sum to
# zero over f. A separate factor g supplied through by replicates this entire
# term; mgcv remains responsible for that replication and its penalties.
#
# There are two stages to mgcv's setup:
#   1. smooth.info() describes the variables and margins before the model frame
#      is built. For a factor by variable, we add a factor recording
#      the observed (f, g) pairs.
#   2. smooth.construct() builds the basis and penalties from the supplied data.
#      In discrete bam, those data have already been compressed by margin.
#
# The extra factor is necessary because independently compressed f and g no
# longer have matching rows. We cannot compare those columns to check whether
# they encode the same grouping. The paired factor retains that information.
# Its basis is just a column of ones: including it in a tensor product changes
# neither the fitted function nor the number of coefficients or penalties.
#
# Keep the coefficient order consistent across X, S, margin, and XP. The order
# is factor contrasts, then continuous margins in the user's covariate order,
# then the constant validation margin (when present).
#
# Regression coverage: tests/tesz.R checks the basis and penalties against
# explicit contrasts; tests/tesz-discrete.R checks both prediction paths, actual
# covariate rounding, and grouping checks after subsetting and NA removal.

#' Sum-to-zero factor interactions with tensor product smooths
#'
#' Constructs a tensor product of two or more one-dimensional continuous
#' margins, with deviations that sum to zero across the levels of one factor.
#'
#' @details
#' Specify the term using `s(x, z, f, bs = "tesz")`, where `f` is a factor.
#' The position of the factor among the covariates does not matter. The smooth
#' is a tensor product despite being specified with [mgcv::s()].
#'
#' Supply continuous-margin options in `xt`: `k`, `bs`, `m`, and `xt` are
#' passed to [mgcv::te()] in the order of the continuous covariates. Defaults
#' are those of `te()` (cubic regression spline margins with dimension 5).
#' Specify marginal `k` and `m` inside `xt`, not in the outer `s()` call.
#'
#' By default (`xt$shared = FALSE`), there is one smoothing parameter per
#' factor level and continuous direction. This follows the level-specific
#' default of mgcv's `sz` basis. These penalties apply to the original
#' level-specific surfaces, not the contrast surfaces. Set `xt$shared = TRUE`
#' for one smoothing parameter per continuous direction, shared across levels.
#'
#' An orthonormal contrast basis absorbs the coefficient sum-to-zero constraint.
#' At every covariate combination, the deviations sum to zero across factor
#' levels with equal weights. The term includes factor offsets; adding the
#' same factor as a parametric effect would duplicate these offsets.
#'
#' A separate factor supplied to `by` replicates the term using mgcv's standard
#' handling, preserving the sum-to-zero constraint within each represented
#' `by` level. Ordered factors omit the first level as usual. The `by` factor
#' must not encode the same grouping as the sum-to-zero factor, independently
#' of labels and level ordering. An internal categorical margin preserves
#' observed pairs of factor levels through model-frame subsetting, missing-value
#' removal, and discretization. The constructor checks equivalence using these
#' pairs, rather than comparing independently discretized factor columns.
#' This auxiliary margin has one constant basis function and introduces no
#' additional coefficients or penalties. No fitting wrapper is required.
#'
#' Both ordinary and discrete [mgcv::bam()] fitting are supported. Discrete
#' fitting stores the factor-contrast margin and each continuous margin
#' separately. Numeric `by` variables, outer `id` and `fx`, multiple sum-to-zero
#' factors, use as a tensor-product margin, and custom plotting are not
#' supported. The coefficient count is `(nlevels(f) - 1) * prod(k)`.
#'
#' @param object A smooth specification from [mgcv::s()] for the constructor,
#'   or a fitted `tesz.smooth` object for the prediction method.
#' @param data A list or data frame containing the covariates and any `by`
#'   variable required by the term.
#' @param knots Optional knots for the continuous margins.
#' @return The constructor returns a `tesz.smooth` object containing the
#'   design matrix, penalties, and information required for prediction.
#'   The prediction method returns the corresponding model matrix.
#' @importFrom mgcv smooth.construct
#' @export
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   x = runif(300), z = runif(300),
#'   f = factor(rep(letters[1:3], 100))
#' )
#' dat$y <- sin(6 * dat$x) + as.integer(dat$f) * dat$z + rnorm(300)
#' fit <- mgcv::gam(
#'   y ~ te(x, z, k = c(4, 4)) +
#'     s(x, z, f, bs = "tesz", xt = list(k = c(4, 4))),
#'   data = dat, method = "REML"
#' )
#' predict(fit, newdata = dat[1:5, ])
#'
#' # A separate factor can replicate the deviation surfaces:
#' # bam(y ~ g + te(x, z, by = g) +
#' #       s(x, z, f, bs = "tesz", by = g), data = dat, discrete = TRUE)
smooth.construct.tesz.smooth.spec <- function(object, data, knots) {
  # The outer s() call selects this constructor. Its xt list holds the te()
  # options for the continuous margins; k and m on s() would be ambiguous.
  # shared is our own option and must not be passed on to te().
  if (!is.null(object$id) || isTRUE(object$fixed)) {
    stop("The tesz basis does not support id or fx arguments")
  }
  if (object$bs.dim != -1 || any(!is.na(object$p.order))) {
    stop("Specify marginal k and m inside xt")
  }
  opt <- object$xt
  if (is.null(opt)) {
    opt <- list()
  }
  if (!is.list(opt)) {
    stop("xt must be a list")
  }
  if (length(opt) && (is.null(names(opt)) || anyNA(names(opt)) ||
    any(names(opt) == "") || anyDuplicated(names(opt)))) {
    stop("xt options must have unique, nonempty names")
  }
  allowed <- c("k", "bs", "m", "xt", "shared")
  if (length(setdiff(names(opt), allowed))) {
    stop("Unknown xt option")
  }
  # Match sz's default: estimate smoothness separately for each factor level.
  shared <- if (is.null(opt$shared)) {
    FALSE
  } else {
    opt$shared
  }
  if (!is.logical(shared) || length(shared) != 1L || is.na(shared)) {
    stop("xt$shared must be TRUE or FALSE")
  }
  # smooth.info() may have appended a variable used only for validation.
  # Exclude it when finding f and the continuous covariates. The user can put
  # f anywhere in the call, but the order of the continuous covariates matters.
  terms <- setdiff(object$term, object$validation.term)
  isfac <- vapply(terms, function(v) {
    is.factor(data[[v]])
  }, logical(1))
  if (sum(isfac) != 1L || sum(!isfac) < 2L) {
    stop("Supply exactly one factor and at least two continuous variables")
  }
  fterm <- terms[isfac]
  vars <- terms[!isfac]
  if (object$by != "NA") {
    f <- data[[fterm]]
    g <- data[[object$by]]
    if (!is.factor(g)) {
      stop("The tesz basis currently supports only factor by variables")
    }
    if (identical(object$by, fterm)) {
      stop(
        "The 'by' factor must not encode the same grouping as the ",
        "sum-to-zero factor"
      )
    }
    # Decode the pair labels actually present in the constructor data, not
    # all levels of the auxiliary factor: unused levels can describe rows
    # removed by subset or na.action. Duplicate pairs do not affect the check.
    # Direct smoothCon() calls can bypass smooth.info(); in that case the
    # original, aligned f and g columns are available for comparison.
    if (!is.null(object$validation.term)) {
      pairs <- tesz_decode_pairs(as.character(data[[object$validation.term]]))
      tesz_check_groups(factor(pairs$f), factor(pairs$g))
    } else {
      tesz_check_groups(f, g)
    }
  }
  if (!all(vapply(vars, function(v) {
    is.numeric(data[[v]]) &&
      is.null(dim(data[[v]]))
  }, logical(1)))) {
    stop("Continuous covariates must be numeric vectors")
  }
  lev <- levels(data[[fterm]])
  if (length(lev) < 2L || any(tabulate(as.integer(data[[fterm]]),
    nbins = length(lev)
  ) == 0L)) {
    stop("Factor must have at least two levels, all observed")
  }
  # H has one row per factor level and q = nlevels(f) - 1 columns. Each column
  # sums to zero, so any linear combination also sums to zero over levels.
  # Normalizing Helmert contrasts gives crossprod(H) = I. This is what allows
  # the shared penalties below to use an identity matrix in the factor part.
  H <- stats::contr.helmert(length(lev))
  H <- sweep(H, 2L, sqrt(colSums(H ^ 2)), "/")
  q <- ncol(H)

  # Let mgcv build the continuous tensor basis, including its directional
  # penalties and any marginal reparameterization. Use smooth.construct(),
  # not smoothCon(), so it does not add a centering constraint to this basis.
  # Keeping the constant function allows deviations in factor-level means.
  args <- opt[intersect(c("k", "bs", "m", "xt"), names(opt))]
  if (is.null(args$bs)) {
    args$bs <- rep("cr", length(vars))
  }
  call <- as.call(c(list(quote(te)), lapply(vars, str2lang), args))
  spec <- eval(call, envir = asNamespace("mgcv"))
  base <- mgcv::smooth.construct(spec, data, knots)
  # For observation i, the row is H[f_i, ] tensor base$X[i, ]. Each contrast
  # therefore has a full copy of the continuous basis. This ordering must
  # also be used by the Kronecker-product penalties and prediction methods.
  Xf <- H[match(as.character(data[[fterm]]), lev), , drop = FALSE]
  object$X <- mgcv::tensor.prod.model.matrix(list(Xf, base$X))

  # For each continuous penalty P, a shared smoothing parameter penalizes
  # the sum of roughness over factor levels. Since crossprod(H) = I, its
  # matrix in contrast coefficients is I_q tensor P.
  if (shared) {
    object$S <- lapply(base$S, function(P) {
      kronecker(diag(q), P)
    })
    object$rank <- q * base$rank
  } else {
    # To allow different smoothness at each level l, start with that level's
    # surface: its coefficients are a combination with weights H[l, ].
    # Its penalty is therefore crossprod(H[l, ]) tensor P. Keep one such
    # penalty per level and direction; mgcv assigns each a smoothing parameter.
    # The factor part has rank one, so this penalty has the same rank as P.
    object$S <- unlist(lapply(seq_along(lev), function(l) {
      G <- crossprod(H[l, , drop = FALSE])
      lapply(base$S, function(P) {
        kronecker(G, P)
      })
    }), recursive = FALSE)
    object$rank <- rep(base$rank, length(lev))
  }
  # With all directional penalties present, each contrast retains a copy of
  # the continuous basis's unpenalized space.
  object$null.space.dim <- q * base$null.space.dim
  object$bs.dim <- object$df <- ncol(object$X)
  # H already enforces the required constraint. A zero-row C tells mgcv not
  # to add its usual sum-to-zero constraint over observations. Also disable
  # automatic nesting constraints, as for mgcv's sz basis.
  object$C <- matrix(0, 0, ncol(object$X))
  object$side.constrain <- FALSE
  # This complete, multiple-penalty smooth cannot itself be a te() margin.
  object$te.ok <- 0
  # No plot method is supplied for the factor-specific surfaces.
  object$plot.me <- FALSE
  # Retain the continuous basis and level mapping for ordinary prediction.
  object$base <- base
  object$factor.term <- fterm
  object$factor.levels <- lev
  object$contrasts <- H
  if (inherits(object, "tensor.smooth.spec")) {
    # bam adds tensor.smooth.spec after seeing tensor.possible in smooth.info().
    # For that path, expose the small marginal matrices so fitting can work
    # with those matrices and row indices instead of the full observation-
    # by-coefficient matrix. Keep the same coefficient order as X and S.
    factor.margin <- list(
      X = Xf, term = fterm, by = "NA",
      levels = lev, contrasts = H,
      C = matrix(0, 0, q), dim = 1L
    )
    class(factor.margin) <- "tesz.factor"
    object$margin <- c(list(factor.margin), base$margin)
    if (!is.null(object$validation.term)) {
      # bam needs a constructed margin for every margin advertised by
      # smooth.info(). The pair-check margin is a column of ones, so adding
      # it leaves X and S unchanged. Do not create coefficients or a penalty
      # for individual (f, g) pairs.
      check.margin <- list(
        X = matrix(1, nrow(Xf), 1L),
        term = object$validation.term, by = "NA",
        dim = 1L, C = matrix(0, 0, 1L)
      )
      class(check.margin) <- "tesz.check"
      object$margin <- c(object$margin, list(check.margin))
    }
    # During fitting, base$margin already contains the reparameterized X
    # matrices. During discrete prediction, mgcv evaluates each raw margin
    # and then applies its saved XP transformation. Prepend NULL because the
    # factor-contrast margin needs no transformation. The final check margin
    # also needs none; it is beyond the end of this XP list.
    object$XP <- c(list(NULL), base$XP)
    class(object) <- c("tesz.smooth", "tensor.smooth")
  } else {
    class(object) <- "tesz.smooth"
  }
  object
}

#' @rdname smooth.construct.tesz.smooth.spec
#' @importFrom mgcv Predict.matrix
#' @export
Predict.matrix.tesz.smooth <- function(object, data) {
  # Ordinary prediction builds the full design in the same order as fitting.
  # Match factor labels to the training levels; newdata's integer factor codes
  # may use a different order. Unknown levels cannot be assigned contrasts.
  # The validation margin need not appear here because it only multiplies by 1.
  # Leave multiplication/replication by the by variable to mgcv.
  i <- match(as.character(data[[object$factor.term]]), object$factor.levels)
  if (anyNA(i)) {
    stop("Missing or unknown factor level in prediction data")
  }
  Xf <- object$contrasts[i, , drop = FALSE]
  B <- mgcv::Predict.matrix(object$base, data)
  mgcv::tensor.prod.model.matrix(list(Xf, B))
}

#' @rdname smooth.construct.tesz.smooth.spec
#' @importFrom mgcv smooth.info
#' @export
smooth.info.tesz.smooth.spec <- function(object) {
  # This method runs before model-frame construction, so factor values are
  # not yet available. Describe the tensor structure here; build it later.
  object$tensor.possible <- TRUE
  if (object$by != "NA" && is.null(object$validation.term)) {
    # Add an expression such as tesz_factor_pairs(g, x, z, f). The helper
    # identifies f when values are available. model.frame() evaluates it and
    # then applies subset and na.action along with the other model variables.
    # Use a namespace-qualified call so prediction does not require attaching
    # mgcvUtils. The guard prevents adding it twice if smooth.info() is reused.
    call <- as.call(c(
      list(quote(mgcvUtils::tesz_factor_pairs)),
      list(str2lang(object$by)), lapply(object$term, str2lang)
    ))
    object$validation.term <- deparse1(call)
    object$term <- c(object$term, object$validation.term)
    object$dim <- length(object$term)
  }
  # All margins are one-dimensional. The additional paired factor is a
  # single categorical variable, so compression preserves its observed pairs
  # exactly while allowing each continuous variable to be discretized alone.
  object$margin <- lapply(object$term, function(term) {
    list(term = term)
  })
  object
}

#' @rdname smooth.construct.tesz.smooth.spec
#' @export
Predict.matrix.tesz.factor <- function(object, data) {
  # Discrete prediction requests each margin separately. Return exactly the
  # contrast rows used during fitting, matching level labels as above.
  i <- match(as.character(data[[object$term]]), object$levels)
  if (anyNA(i)) {
    stop("Missing or unknown factor level in prediction data")
  }
  object$contrasts[i, , drop = FALSE]
}

# Compare observed partitions, independently of labels and level ordering.
tesz_check_groups <- function(f, g) {
  if (!is.factor(g)) {
    stop("The tesz basis currently supports only factor by variables")
  }
  # Number groups in order of first appearance. For example, both
  # c("a", "a", "b") and c("Y", "Y", "X") become c(1, 1, 2).
  # Equal codes mean equal group membership, regardless of labels or level
  # ordering. This checks equality only, not nesting or general estimability.
  if (identical(match(f, unique(f)), match(g, unique(g)))) {
    stop(
      "The 'by' factor must not encode the same grouping as the ",
      "sum-to-zero factor"
    )
  }
  invisible(NULL)
}


#' Internal factor-pair encoding for tesz smooths
#'
#' Used automatically in model-frame expressions generated by the `tesz`
#' smooth. Exported so those expressions also work when the package is loaded
#' but not attached. Factor labels carry the encoding because discretization
#' does not preserve arbitrary attributes on factor columns.
#'
#' @param g The factor `by` variable.
#' @param ... The original smooth covariates, exactly one of which is a factor.
#' @return A factor encoding observed pairs, with missing pairs left missing.
#' @keywords internal
#' @export
tesz_factor_pairs <- function(g, ...) {
  # smooth.info() cannot know which covariate is f because it has no data.
  # Find it here when the model-frame expression is evaluated. The numeric
  # covariates are used only to locate f; they do not enter the pair encoding.
  inputs <- list(...)
  f <- inputs[vapply(inputs, is.factor, logical(1))]
  if (length(f) != 1L) {
    stop("Supply exactly one factor and at least two continuous variables")
  }
  if (!is.factor(g)) {
    stop("The tesz basis currently supports only factor by variables")
  }
  f <- f[[1L]]
  # Encode each pair as <length of f>:<f><g>.
  # For example, f = "a:b" and g = "c" become "3:a:bc":
  #   Read "3:", then take the next three characters, "a:b", as f.
  #   Everything left, "c", is g; we do not need to store its length.
  # The length of f fixes the boundary even when either label contains colons,
  # empty strings, or Unicode text.
  # Store this in factor labels: bam's compression can drop custom attributes.
  key <- paste0(nchar(as.character(f)), ":", as.character(f), as.character(g))
  # paste0() turns missing values into the text "NA". Restore actual NAs so
  # the model frame can apply the user's missing-value policy correctly.
  key[is.na(f) | is.na(g)] <- NA_character_
  factor(key)
}
tesz_decode_pairs <- function(key) {
  # Decode labels produced by tesz_factor_pairs(). The first colon ends the
  # length field; that length tells us where f ends, regardless of its content.
  end <- regexpr(":", key, fixed = TRUE)
  n <- as.integer(substr(key, 1, end - 1))
  f <- substr(key, end + 1, end + n)
  # Once f has been read, all remaining characters belong to g.
  g <- substring(key, end + n + 1)
  data.frame(f = f, g = g)
}
#' @rdname smooth.construct.tesz.smooth.spec
#' @export
Predict.matrix.tesz.check <- function(object, data) {
  # This margin exists for the fitting-time grouping check, not to model
  # pair-specific effects. Return 1 for every prediction row, including new
  # combinations of known levels. Do not repeat the equivalence check on
  # prediction data: a small grid or a single row can make f and g look equal.
  matrix(1, length(data[[object$term]]), 1L)
}
