#' Calculate Cauchy Combination Test Statistic
#'
#' This function calculates the Cauchy Combination Test (CCT) statistic for a given set of p-values and optional weights.
#'
#' @param p_values A numeric vector of p-values.
#' @param weights A numeric vector of weights corresponding to the p-values. If NULL, equal weights are used.
#' @return A numeric value representing the CCT statistic.
#' @details The CCT statistic is calculated as a weighted sum of the tangents of the p-values (transformed).
#' If any p-value is zero, the statistic is zero. If any p-value is very close to one, the statistic is calculated
#' using the minimum p-value approach.
#' @examples
#' p_values <- c(0.01, 0.02, 0.03, 0.04)
#' weights <- c(0.4, 0.3, 0.2, 0.1)
#' cct_stat <- cauchy_statistic(p_values, weights)
#' print(cct_stat)
#' @export
cauchy_statistic <- function(p_values, weights=NULL) {
  is.zero <- sum(p_values == 0) >= 1
  is.one <- sum(p_values > (1 - 1e-14)) >= 1

  if (is.zero) {
    return(0)
  }

  if (is.one) {
    p <- min(p_values) * length(p_values)
    if (p > 1) {
      return(Inf)
    } else {
      return(qcauchy(p, lower.tail=FALSE))
    }
  }

  if (is.null(weights)) {
    weights <- rep(1 / length(p_values), length(p_values))
  } else if (length(weights) != length(p_values)) {
    stop("The length of weights should be the same as that of the p-values!")
  } else if (sum(weights < 0) > 0) {
    stop("All the weights must be positive!")
  } else {
    weights <- weights / sum(weights)
  }

  is_small <- (p_values <= 1e-16)
  if (sum(is_small) == 0) {
    cct_stat <- sum(weights * tan((0.5 - p_values) * pi))
  } else {
    cct_stat <- sum(weights[is_small] / p_values[is_small] / pi)
    cct_stat <- cct_stat + sum(weights[!is_small] * tan((0.5 - p_values[!is_small]) * pi))
  }

  return(cct_stat)
}
