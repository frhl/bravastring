#' Get Expected P-values
#'
#' This function calculates the expected p-values based on observed p-values.
#'
#' @param observed_p A numeric vector of observed p-values.
#' @param na.rm A logical value indicating whether NA values should be removed. Default is FALSE.
#'
#' @return A numeric vector of expected p-values.
#' @export
#'
#' @examples
#' observed_p <- c(0.01, 0.05, 0.03, NA)
#' get_expected_p(observed_p, na.rm = TRUE)
get_expected_p <- function(observed_p, na.rm = FALSE){
  stopifnot(is.numeric(observed_p))
  stopifnot(any(observed_p >= 0))
  sum_na <- sum(is.na(observed_p))
  if (sum_na > 0){
    if (na.rm){
      observed_p <- observed_p[!is.na(observed_p)]
    } else {
      warning("NAs detected in P-values. Set na.rm = TRUE to remove them.")
    }
  }
  n <- length(observed_p)
  observed_rank <- rank(observed_p)
  uniform <- (1:n)/(n+1)
  uniform <- uniform[observed_rank]
  return(uniform)
}

