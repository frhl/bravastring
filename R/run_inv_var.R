#' Perform Inverse-Variance Weighted Meta-Analysis
#'
#' This function performs an inverse-variance weighted meta-analysis on a data table.
#' It calculates the meta-analysis effect size (beta), its standard error, and the p-value.
#'
#' @param dt A data.table containing the data for the meta-analysis.
#' @param se_cols A character vector of column names in \code{dt} representing the standard errors of the effect sizes.
#' @param beta_cols A character vector of column names in \code{dt} representing the effect sizes.
#' @return The input data.table \code{dt} with additional columns for the meta-analysis p-value.
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Checks that the provided \code{se_cols} and \code{beta_cols} are present in \code{dt} and that their lengths are equal.
#'   \item Calculates weights as the inverse of the squared standard errors.
#'   \item Computes weighted effect sizes.
#'   \item Sums the weights and the weighted effect sizes.
#'   \item Calculates the meta-analysis effect size (\code{beta_meta}) and its standard error (\code{se_meta}).
#'   \item Computes the z-value and the p-value for the meta-analysis.
#'   \item Appends the meta-analysis p-value to the original data table.
#' }
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   se1 = c(0.1, 0.2, 0.1),
#'   se2 = c(0.2, 0.1, 0.2),
#'   beta1 = c(0.3, 0.5, 0.2),
#'   beta2 = c(0.4, 0.3, 0.1)
#' )
#' se_cols <- c("se1", "se2")
#' beta_cols <- c("beta1", "beta2")
#' result <- run_inv_var(dt, se_cols, beta_cols)
#' print(result)
#' }
#' @import data.table
#' @importFrom stats pnorm
#' @export
run_inv_var <- function(dt, se_cols, beta_cols){
  stopifnot(se_cols %in% colnames(dt))
  stopifnot(beta_cols %in% colnames(dt))
  stopifnot(length(se_cols) == length(beta_cols))
  
  # Calculate weights and weighted effect sizes
  weights <- dt[, lapply(se_cols, function(se) 1/(get(se)^2))]
  weighted_effects <- dt[, lapply(seq_along(beta_cols), function(i) get(beta_cols[i]) * weights[[i]])]
  
  # Sum weights and weighted effects
  total_weight <- Reduce("+", weights)
  total_effect <- Reduce("+", weighted_effects)
  
  # Calculate meta-analysis beta and its standard error
  beta_meta <- total_effect / total_weight
  se_meta <- sqrt(1 / total_weight)
  
  # Compute the meta-analysis p-value
  z_value <- beta_meta / se_meta
  p_value_meta <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  # Append results to the original data table
  dt$p_value_meta <- p_value_meta
  return(dt)
}

