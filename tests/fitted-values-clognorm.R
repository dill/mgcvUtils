library(mgcvUtils)

if (requireNamespace("gratia", quietly = TRUE)) {
  near <- function(x, y, tol = 1e-8) {
    stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol)))
  }
  set.seed(42)
  d <- data.frame(x = seq(0, 1, length.out = 100))
  y <- 1 + d$x + rnorm(nrow(d), sd = 0.3)
  for (base in list("e", 2, 10)) for (nc in c(0L, 20L)) {
    censored <- rank(y) <= nc
    limit <- if (nc) sort(y)[nc] else min(y)
    d$y <- cbind(ifelse(censored, limit, y), ifelse(censored, -Inf, y))
    b <- gam(y ~ s(x, k = 6), data = d,
      family = clognorm(base = base), method = "REML")
    for (nd in list(NULL, data.frame(x = c(0.2, 0.5, 0.8), .row = 11:13))) {
      for (level in c(0.8, 0.95)) {
        crit <- qnorm((1 + level) / 2)
        args <- list(object = b, se.fit = TRUE)
        if (!is.null(nd)) args$newdata <- nd
        for (scale in c("response", "link", "linear predictor")) {
          got <- fitted_values_clognorm(b, data = nd, scale = scale,
            ci_level = level, block.size = 2)
          type <- if (scale == "response") "response" else "link"
          expected <- do.call(predict, c(args, list(type = type)))
          near(got$.fitted, expected$fit)
          near(got$.se, expected$se.fit)
          stopifnot(all(is.finite(unlist(got))),
            all(got$.lower_ci < got$.fitted), all(got$.upper_ci > got$.fitted))
          if (scale == "response") {
            # On the response scale, a log interval is multiplicatively
            # symmetric and its log half-width is crit * relative SE.
            near(log(got$.upper_ci / got$.fitted), crit * got$.se / got$.fitted)
            near(log(got$.fitted / got$.lower_ci), crit * got$.se / got$.fitted)
          } else {
            near(got$.lower_ci, expected$fit - crit * expected$se.fit)
            near(got$.upper_ci, expected$fit + crit * expected$se.fit)
          }
          near(got$.row, if (is.null(nd)) seq_len(nrow(d)) else nd$.row)
        }
      }
    }
  }
} else {
  message("Skipping fitted_values_clognorm tests: gratia is not installed")
}
