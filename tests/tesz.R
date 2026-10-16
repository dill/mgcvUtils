library(mgcv)
library(mgcvUtils)
set.seed(42)
d <- data.frame(
  x = runif(600), z = runif(600), w = runif(600),
  f = factor(sample(letters[1:3], 600, TRUE))
)
d$y <- sin(6 * d$x) + cos(4 * d$z) +
  (as.integer(d$f) - 2) * (1 + sin(5 * d$x) * cos(3 * d$z)) +
  rnorm(600, sd = 0.3)
# The explicit contrast model below uses shared smoothing parameters.
a <- gam(
  y ~ te(x, z, k = c(5, 5)) +
    s(x, z, f, bs = "tesz", xt = list(k = c(5, 5), shared = TRUE)),
  data = d, method = "REML"
)
stopifnot(a$rank == length(coef(a)), length(a$sp) == 4)
s <- a$smooth[[2]]
H <- s$contrasts
d$c1 <- H[as.integer(d$f), 1]
d$c2 <- H[as.integer(d$f), 2]
b <- gam(y ~ te(x, z, k = c(5, 5)) +
  te(x, z, by = c1, k = c(5, 5), id = 1) +
  te(x, z, by = c2, k = c(5, 5), id = 1), data = d, method = "REML")
err <- max(abs(predict(a) - predict(b)))
stopifnot(err < 1e-5)
nd <- expand.grid(
  x = seq(0, 1, length.out = 7), z = seq(0, 1, length.out = 7),
  f = levels(d$f)
)
dev <- predict(a, nd, type = "terms")[, 2]
sumerr <- max(abs(rowSums(matrix(dev, ncol = 3))))
stopifnot(sumerr < 1e-10)
raw <- smoothCon(s(x, z, f, bs = "tesz", xt = list(k = c(5, 5))), d)[[1]]
stopifnot(max(abs(Predict.matrix(raw, d) - raw$X)) < 1e-12)
# The default has two directional penalties per level, plus two for te().
c <- gam(
  y ~ te(x, z, k = c(5, 5)) +
    s(x, z, f, bs = "tesz", xt = list(k = c(5, 5))),
  data = d, method = "REML"
)
stopifnot(
  c$rank == length(coef(c)), length(c$sp) == 8,
  all(is.finite(predict(c, nd, se.fit = TRUE)$se.fit))
)
r <- smoothCon(s(x, z, f, bs = "tesz", xt = list(k = c(5, 5), shared = FALSE)),
  d,
  scale.penalty = FALSE
)[[1]]
# Omitting shared, including when xt is absent, must equal shared = FALSE.
for (spec in list(s(x, z, f, bs = "tesz"),
                  s(x, z, f, bs = "tesz", xt = list(k = c(5, 5))))) {
  default <- smoothCon(spec, d, scale.penalty = FALSE)[[1]]
  stopifnot(
    length(default$S) == 2L * nlevels(d$f),
    identical(default$X, r$X), identical(default$S, r$S),
    identical(default$rank, r$rank),
    identical(default$null.space.dim, r$null.space.dim)
  )
}
theta <- rnorm(ncol(r$X))
p <- ncol(r$base$X)
alpha <- matrix(theta, nrow = p) %*% t(r$contrasts)
for (l in 1:3) {
  for (j in 1:2) {
    left <- drop(crossprod(theta, r$S[[(l - 1) * 2 + j]] %*% theta))
    right <- drop(crossprod(alpha[, l], r$base$S[[j]] %*% alpha[, l]))
    stopifnot(abs(left - right) < 1e-9)
  }
}
e <- gam(
  y ~ te(x, z, w, k = c(4, 4, 4)) +
    s(x, z, w, f, bs = "tesz", xt = list(k = c(4, 4, 4))),
  data = d, method = "REML"
)
stopifnot(
  e$rank == length(coef(e)), length(e$sp) == 12,
  all(is.finite(predict(e, d[1:10, ], se.fit = TRUE)$se.fit))
)
cat(
  "mgcv", as.character(packageVersion("mgcv")), "\n",
  "Contrast-by equivalence, max prediction difference:", err, "\n",
  "Level sum, max absolute deviation:", sumerr, "\n",
  "Shared, per-level and 3-continuous-margin fits: full rank\n",
  "Per-level penalty quadratic forms and prediction matrices: passed\n"
)

# Factor-by replication, including ordered factors and per-level penalties.
d$g <- factor(rep(c("a", "b"), length.out = nrow(d)))
for (shared in c(TRUE, FALSE)) {
  for (ordered_by in c(FALSE, TRUE)) {
    d$g <- factor(d$g, ordered = ordered_by)
    byfit <- gam(
      y ~ g + te(x, z, by = g, k = c(4, 4)) +
        s(x, z, f, bs = "tesz", by = g,
          xt = list(k = c(4, 4), shared = shared)),
      data = d, method = "REML"
    )
    stopifnot(byfit$rank == length(coef(byfit)))
    grid <- expand.grid(
      x = c(.2, .7), z = c(.3, .8), f = levels(d$f),
      g = levels(d$g)
    )
    grid$f <- factor(grid$f, levels = rev(levels(d$f)))
    grid$g <- factor(grid$g, levels = levels(d$g), ordered = ordered_by)
    terms <- predict(byfit, grid, type = "terms")
    idx <- which(vapply(byfit$smooth, inherits, logical(1), "tesz.smooth"))
    for (j in idx) {
      dev <- terms[, byfit$smooth[[j]]$label]
      stopifnot(max(abs(apply(array(dev, c(4, 3, 2)), c(1, 3), sum))) < 1e-10)
      inactive <- as.character(grid$g) != byfit$smooth[[j]]$by.level
      stopifnot(max(abs(dev[inactive])) < 1e-10)
    }
    stopifnot(all(is.finite(predict(byfit, grid, se.fit = TRUE)$se.fit)))
  }
}
expect_error <- function(expr, text) {
  err <- tryCatch(force(expr), error = identity)
  stopifnot(
    inherits(err, "error"),
    grepl(text, conditionMessage(err), fixed = TRUE)
  )
}
# Direct reuse, a literal copy, reordered levels, and relabelled partitions.
expect_error(smoothCon(s(x, z, f, bs = "tesz", by = f), d), "same grouping")
for (g in list(
  d$f, factor(d$f, levels = rev(levels(d$f))),
  factor(c("z", "x", "y")[as.integer(d$f)])
)) {
  d$g <- g
  expect_error(smoothCon(s(x, z, f, bs = "tesz", by = g), d), "same grouping")
}
d$g <- seq_len(nrow(d))
expect_error(smoothCon(s(x, z, f, bs = "tesz", by = g), d), "only factor by")
expect_error(
  smoothCon(s(x, z, f, bs = "tesz", xt = list(shared = NA)), d),
  "shared must be TRUE or FALSE"
)
cat(
  "Factor-by replication, ordered factors, and equivalent-group rejection:",
  "passed\n"
)
