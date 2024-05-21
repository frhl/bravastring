#' Calculate Inflation Factor
#'
#' This function calculates the inflation factor for a given set of p-values.
#'
#' @param P A numeric vector of p-values.
#' @param cutoff A numeric value for the cutoff to use in the chi-squared distribution. Default is 0.5.
#'
#' @return A numeric value representing the inflation factor.
#' @export
#'
#' @examples
#' p_values <- c(0.05, 0.01, 0.03, 0.2)
#' calc_inflation(p_values)
calc_inflation <- function(P, cutoff=0.5){
  return(median(qchisq(1-P, 1)) / qchisq(cutoff, 1))
}

