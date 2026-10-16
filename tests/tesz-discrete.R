library(mgcv)
library(mgcvUtils)

expect_error <- function(expr, text) {
  err <- tryCatch(force(expr), error = identity)
  stopifnot(
    inherits(err, "error"),
    grepl(text, conditionMessage(err), fixed = TRUE)
  )
}

# Pair encoding must be reversible and must not merge different label pairs.
# Include delimiter collisions, empty labels, Unicode, digits and literal NA.
labels <- c("a:b", "a", "b:c", "", "\u03b2 | two", "3:", "NA")
pairs <- expand.grid(
  f = labels, g = labels, stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
)
encoded <- tesz_factor_pairs(factor(pairs$g), factor(pairs$f))
decoded <- mgcvUtils:::tesz_decode_pairs(as.character(encoded))
stopifnot(
  identical(decoded, pairs), anyDuplicated(as.character(encoded)) == 0L,
  identical(
    as.character(tesz_factor_pairs(factor("c"), factor("a:b"))),
    "3:a:bc"
  )
)
missing <- tesz_factor_pairs(
  factor(c("g", NA, "NA")),
  factor(c(NA, "f", "NA"))
)
stopifnot(identical(is.na(missing), c(TRUE, TRUE, FALSE)))

set.seed(2026)
dat <- expand.grid(
  x = seq(0, 1, length.out = 8),
  z = seq(0, 1, length.out = 7),
  f = factor(c("a", "b", "c")),
  g = factor(c("first", "second"))
)
dat$y <- sin(6 * dat$x) + cos(4 * dat$z) +
  (as.integer(dat$f) - 2) * (1 + dat$x * dat$z) +
  (as.integer(dat$g) - 1) * dat$x + rnorm(nrow(dat), sd = 0.2)
knots <- list(
  x = seq(0, 1, length.out = 4),
  z = seq(0, 1, length.out = 5)
)

# Fixed penalties and knots make the exact-grid discrete/dense comparison
# independent of numerical differences in smoothing-parameter optimization.
for (shared in c(TRUE, FALSE)) {
  for (ordered_by in c(FALSE, TRUE)) {
    dat$g <- factor(dat$g, ordered = ordered_by)
    form <- y ~ g + te(x, z, by = g, k = c(4, 5)) +
      s(x, f, z,
        bs = "tesz", by = g,
        xt = list(k = c(4, 5), shared = shared)
      )
    groups <- if (ordered_by) {
      1L
    } else {
      2L
    }
    sp <- rep(0.7, groups * (2 + if (shared) {
      2
    } else {
      6
    }))
    dense <- bam(form,
      data = dat, knots = knots, sp = sp,
      control = gam.control(scalePenalty = FALSE)
    )
    disc <- bam(form,
      data = dat, knots = knots, sp = sp, discrete = TRUE,
      control = gam.control(scalePenalty = FALSE)
    )
    stopifnot(
      length(coef(disc)) == length(coef(dense)),
      max(abs(predict(disc) - predict(dense))) < 1e-7,
      max(abs(predict(disc, discrete = FALSE) -
        disc$linear.predictors)) < 1e-8
    )
    grid <- expand.grid(
      x = c(0.17, 0.65), z = c(0.21, 0.83),
      f = rev(levels(dat$f)), g = levels(dat$g)
    )
    grid$f <- factor(grid$f, levels = rev(levels(dat$f)))
    grid$g <- factor(grid$g, levels = levels(dat$g), ordered = ordered_by)
    p1 <- predict(disc, grid, se.fit = TRUE)
    p2 <- predict(disc, grid, se.fit = TRUE, discrete = FALSE)
    stopifnot(
      max(abs(p1$fit - p2$fit)) < 1e-8,
      max(abs(p1$se.fit - p2$se.fit)) < 1e-8
    )
    X <- predict(disc, grid, type = "lpmatrix", discrete = FALSE)
    stopifnot(max(abs(drop(X %*% coef(disc)) - p1$fit)) < 1e-8)
    term <- predict(disc, grid, type = "terms")
    for (sm in disc$smooth) {
      if (!inherits(sm, "tesz.smooth")) {
        next
      }
      stopifnot(
        inherits(sm, "tensor.smooth"), length(sm$margin) == 4L,
        ncol(sm$margin[[4L]]$X) == 1L,
        all(sm$margin[[4L]]$X == 1),
        sm$last.para - sm$first.para + 1L == 2 * 4 * 5
      )
      values <- term[, sm$label]
      stopifnot(
        max(abs(apply(array(values, c(4, 3, 2)), c(1, 3), sum))) < 1e-9,
        max(abs(values[as.character(grid$g) != sm$by.level])) < 1e-9
      )
    }
  }
}

# More than two continuous margins, without by; mixed bases exercise the
# continuous marginal reparameterization on both prediction paths.
dat$w <- runif(nrow(dat))
three <- bam(
  y ~ te(x, z, w, k = c(4, 4, 4)) +
    s(f, x, z, w,
      bs = "tesz",
      xt = list(k = c(4, 4, 4), bs = c("cr", "ps", "cr"))
    ),
  data = dat, discrete = TRUE
)
# Three common-surface penalties plus three per factor level by default.
stopifnot(length(three$sp) == 3L + 3L * nlevels(dat$f))
new <- dat[seq(1, nrow(dat), length.out = 20), ]
p1 <- predict(three, new, se.fit = TRUE)
p2 <- predict(three, new, se.fit = TRUE, discrete = FALSE)
stopifnot(
  max(abs(p1$fit - p2$fit)) < 1e-8,
  max(abs(p1$se.fit - p2$se.fit)) < 1e-8,
  length(three$smooth[[2]]$margin) == 4L
)

form <- y ~ g + te(x, z, by = g, k = c(4, 4)) +
  s(x, z, f, bs = "tesz", by = g, xt = list(k = c(4, 4)))
dat$g <- factor(dat$g, ordered = FALSE)
# Includes labels containing delimiters, spaces, empty strings and Unicode.
levels(dat$f) <- c("a:1", "", "\u03b2 | two")
for (g in list(
  dat$f, factor(dat$f, levels = rev(levels(dat$f))),
  factor(c("G:1", "G|2", "G 3")[as.integer(dat$f)])
)) {
  dat$g <- g
  expect_error(bam(form, data = dat, discrete = TRUE), "same grouping")
}
expect_error(bam(y ~ s(x, z, f, bs = "tesz", by = f),
  data = dat, discrete = TRUE
), "same grouping")

# One exceptional pair prevents equivalence in the full data. Omitting it
# by subset, response NA, another predictor NA, weights NA or offset NA must
# expose equivalence. Check setup only to avoid fitting an unidentified model.
dat$g <- factor(c("G:1", "G|2", "G 3")[as.integer(dat$f)])
dat$g[1] <- "G|2"
ok <- bam(form, data = dat, discrete = TRUE, fit = FALSE)
stopifnot(is.list(ok))
expect_error(bam(form,
  data = dat, subset = seq_len(nrow(dat)) > 1,
  discrete = TRUE, fit = FALSE
), "same grouping")
dat$y[1] <- NA
expect_error(
  bam(form, data = dat, discrete = TRUE, fit = FALSE),
  "same grouping"
)
dat$y[1] <- 0
dat$extra <- rep(0, nrow(dat))
dat$extra[1] <- NA
expect_error(bam(update(form, . ~ . + extra),
  data = dat,
  discrete = TRUE, fit = FALSE
), "same grouping")
dat$wt <- rep(1, nrow(dat))
dat$wt[1] <- NA
expect_error(bam(form,
  data = dat, weights = wt,
  discrete = TRUE, fit = FALSE
), "same grouping")
dat$off <- rep(0, nrow(dat))
dat$off[1] <- NA
expect_error(bam(form,
  data = dat, offset = off,
  discrete = TRUE, fit = FALSE
), "same grouping")

# Keep observed partitions distinct despite independently compressed factors
# having the same number of levels, and allow predictions on a single pair.
dat$g <- factor(rep(c("G:1", "G|2", "G 3"), length.out = nrow(dat)))
valid <- bam(form, data = dat, discrete = TRUE)
# Each by level has two common-surface penalties and two per level of f.
stopifnot(length(valid$sp) == nlevels(dat$g) * (2L + 2L * nlevels(dat$f)))
one <- predict(valid, dat[1, ], se.fit = TRUE)
stopifnot(all(is.finite(c(one$fit, one$se.fit))))

# Force actual rounding, not just exact compression of repeated values.
rounded <- dat
rounded$x <- runif(nrow(rounded))
rounded$z <- runif(nrow(rounded))
coarse <- bam(form,
  data = rounded, discrete = 20,
  knots = list(
    x = seq(0, 1, length.out = 4),
    z = seq(0, 1, length.out = 4)
  )
)
# Reconstruct the univariate grid used by mgcv for 20 bins. Ordinary
# prediction at these rounded covariates must reproduce the fitted values.
for (v in c("x", "z")) {
  bounds <- range(rounded[[v]])
  delta <- diff(bounds) / 19
  rounded[[v]] <- bounds[1] + round((rounded[[v]] - bounds[1]) / delta) * delta
}
stopifnot(max(abs(predict(coarse, rounded, discrete = FALSE) -
  coarse$linear.predictors)) < 1e-8)

# The generated expression is namespace-qualified, so attachment is unnecessary.
detach("package:mgcvUtils")
unattached <- bam(form, data = dat, discrete = TRUE)
stopifnot(max(abs(predict(unattached) - predict(valid))) < 1e-8)
cat("Discrete tesz fitting, prediction, and paired-factor validation: passed\n")
