# Evaluate the signed difference s1 * exp(a1) * dnorm(x1) -
# s0 * exp(a0) * dnorm(x0), keeping zero terms in their original order.
# This implementation is independent of mgcv's local ddnorm helper.
.clognorm_ddnorm <- function(x0, x1, a0 = 0, a1 = 0, s0 = 1, s1 = 1,
                             log.p = TRUE) {
  if (any(x0 >= x1)) warning("some x0>=x1")
  n <- length(x0)
  p0 <- dnorm(x0, log = TRUE) + rep_len(a0, n)
  p1 <- dnorm(x1, log = TRUE) + rep_len(a1, n)
  # The subtraction reverses the sign of the first endpoint's term.
  t0 <- -rep_len(s0, n)
  t1 <- rep_len(s1, n)
  t0[p0 == -Inf] <- 0
  t1[p1 == -Inf] <- 0
  magnitude <- rep(-Inf, n)
  direction <- numeric(n)

  only0 <- t0 != 0 & t1 == 0
  only1 <- t1 != 0 & t0 == 0
  magnitude[only0] <- p0[only0]
  direction[only0] <- t0[only0]
  magnitude[only1] <- p1[only1]
  direction[only1] <- t1[only1]

  both <- which(t0 != 0 & t1 != 0)
  if (length(both)) {
    hi <- pmax(p0[both], p1[both])
    lo <- pmin(p0[both], p1[both])
    same <- t0[both] == t1[both]
    j <- both[same]
    magnitude[j] <- hi[same] + log1p(exp(lo[same] - hi[same]))
    direction[j] <- t0[j]

    j <- both[!same]
    magnitude[j] <- hi[!same] + log(-expm1(lo[!same] - hi[!same]))
    direction[j] <- ifelse(p0[j] > p1[j], t0[j], t1[j])
    direction[j[p0[j] == p1[j]]] <- 0
  }
  if (log.p) {
    attr(magnitude, "sign") <- direction
    magnitude
  } else {
    direction * exp(magnitude)
  }
}

.clognorm_cnorm_compat <- function(Dd, version = utils::packageVersion("mgcv")) {
  # Limit this compatibility change to released versions through 1.9-4.
  # Revisit the boundary after the next mgcv release: it does not imply that
  # every development version above 1.9-4 already contains the complete fix.
  if (utils::compareVersion(as.character(version), "1.9-4") > 0L) return(Dd)

  code <- body(Dd)
  assignments <- which(vapply(as.list(code), function(expr) {
    is.call(expr) && length(expr) == 3L &&
      identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("ddnorm"))
  }, logical(1)))
  if (length(assignments) != 1L) {
    stop("Cannot apply clognorm() compatibility fix: unexpected mgcv cnorm() derivative layout.",
         call. = FALSE)
  }
  rhs <- code[[assignments]][[3L]]
  if (identical(rhs, .clognorm_ddnorm)) return(Dd)
  if (!is.call(rhs) || !identical(rhs[[1L]], as.name("function"))) {
    stop("Cannot apply clognorm() compatibility fix: unexpected mgcv cnorm() helper layout.",
         call. = FALSE)
  }
  # Replace only the helper in this local callback copy. Embedding the function
  # preserves the callback's original family environment and avoids namespace
  # changes, textual code substitution, or distributing a copy of mgcv's Dd.
  code[[assignments]][[3L]] <- .clognorm_ddnorm
  body(Dd) <- code
  Dd
}
