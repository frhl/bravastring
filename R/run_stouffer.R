#' Perform Stouffer's Z-score Method for Meta-Analysis
#'
#' This function performs Stouffer's Z-score method for meta-analysis on a data table.
#' It calculates a weighted Z-score and the corresponding p-value.
#'
#' @param dt A data.table containing the data for the meta-analysis.
#' @param n_eff_cols A character vector of column names in \code{dt} representing the effective sample sizes.
#' @param p_cols A character vector of column names in \code{dt} representing the p-values.
#' @param beta_cols A character vector of column names in \code{dt} representing the effect sizes.
#' @param two_tail A logical value indicating whether to perform a two-tailed test (default is TRUE).
#' @param nudge_p A logical value indicating whether to adjust p-values that are close to 1 (default is TRUE).
#' @return The input data.table \code{dt} with additional columns for the weighted Z-score and the meta-analysis p-value.
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Optionally nudges p-values that are close to 1 to avoid issues with the qnorm function.
#'   \item Adjusts p-values for two-tailed tests if specified.
#'   \item Calculates the numerator for the weighted Z-scores using the effective sample sizes, p-values, and effect sizes.
#'   \item Computes the weighted Z-scores.
#'   \item Calculates the meta-analysis p-values based on the weighted Z-scores.
#' }
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   n_eff1 = c(100, 200, 150),
#'   n_eff2 = c(200, 150, 100),
#'   p1 = c(0.05, 0.01, 0.03),
#'   p2 = c(0.02, 0.03, 0.01),
#'   beta1 = c(0.3, -0.5, 0.2),
#'   beta2 = c(0.4, 0.3, -0.1)
#' )
#' n_eff_cols <- c("n_eff1", "n_eff2")
#' p_cols <- c("p1", "p2")
#' beta_cols <- c("beta1", "beta2")
#' result <- run_stouffer(dt, n_eff_cols, p_cols, beta_cols)
#' print(result)
#' }
#' @import data.table
#' @importFrom stats pnorm qnorm
#' @export
run_stouffer <- function(dt, n_eff_cols, p_cols, beta_cols, two_tail=TRUE, nudge_p=TRUE){
  
  # Nudge P-values that are close to exactly 1
  if (nudge_p){
    dt[, (p_cols) := lapply(.SD, function(x) ifelse(x > 0.99, 0.99, x)), .SDcols = p_cols]
  }
  
  # Prepare if two-sided or one-tailed
  if (two_tail){
    dt[, (p_cols) := .SD * 0.5, .SDcols = p_cols]
  } else {
    dt[, beta_dummy := 1]
    beta_cols <- "beta_dummy"
  }
  
  # Get numerator
  weighted_z_numerator <- sapply(dt[, ..n_eff_cols], sqrt) *
    (sapply(dt[, ..p_cols], qnorm) * (-1)) *
    (sapply(dt[, ..beta_cols], sign))
  
  # Get weighted Z
  dt$z_weighted <- apply(weighted_z_numerator, 1, sum) /
    apply(dt[, ..n_eff_cols], 1, function(x) sqrt(sum(x)))
  
  # Get meta p-value
  if (two_tail) {
    dt$p_value_meta <- 2 * pnorm(abs(dt$z_weighted), lower.tail = FALSE)
  } else {
    dt$p_value_meta <- pnorm(dt$z_weighted, lower.tail = FALSE)
  }
  
  return(dt)
}

