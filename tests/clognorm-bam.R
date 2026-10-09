library(mgcvUtils)

set.seed(42)
d <- data.frame(x = seq(0, 1, length.out = 100))
y <- 1 + d$x + rnorm(nrow(d), sd = 0.3)
message <- "clognorm() does not support bam(); use mgcv::gam() instead."
expect_bam_error <- function(expr) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), identical(conditionMessage(err), message))
}

for (censored in c(FALSE, TRUE)) {
  d$y <- if (censored) cbind(pmax(y, 1.2), ifelse(y < 1.2, -Inf, y)) else y
  for (discrete in c(FALSE, TRUE)) {
    expect_bam_error(mgcv::bam(y ~ s(x, k = 6), data = d,
      family = clognorm(base = "e"), discrete = discrete, method = "fREML"))
  }
  expect_bam_error(mgcv::bam(y ~ s(x, k = 6), data = d,
    family = clognorm(base = "e"), method = "REML"))
  # A family constructed before fitting must be caught too, including aliases.
  fam <- clognorm(base = "e")
  fit <- mgcv::bam
  expect_bam_error(fit(y ~ s(x, k = 6), data = d, family = fam))
  expect_bam_error(do.call(fit, list(formula = y ~ s(x, k = 6),
    data = d, family = fam)))
  for (discrete in c(FALSE, TRUE)) {
    setup <- mgcv::bam(y ~ s(x, k = 6), data = d, family = fam,
      discrete = discrete, fit = FALSE)
    expect_bam_error(mgcv::bam(G = setup))
  }
  # Rejection must not prevent reusing the family for supported gam fitting.
  model <- mgcv::gam(y ~ s(x, k = 6), data = d, family = fam, method = "REML")
  stopifnot(all(is.finite(predict(model, type = "response"))))
}
