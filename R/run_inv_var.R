#' Perform Inverse Variance Meta-Analysis
#'
#' @param grouped_dt A grouped data table or data frame.
#' @param input_beta_name Character. Name of the column containing input beta coefficients.
#' @param input_se_name Character. Name of the column containing input standard errors.
#' @param output_beta_meta Character. Name for the output meta-analysis beta column.
#' @param output_se_meta Character. Name for the output meta-analysis standard error column.
#' @param output_meta_pvalue Character. Name for the output meta-analysis p-value column.
#'
#' @return A data table with added columns for meta-analysis results:
#'   \item{output_beta_meta}{The calculated meta-analysis beta coefficient.}
#'   \item{output_se_meta}{The calculated meta-analysis standard error.}
#'   \item{output_meta_pvalue}{The calculated meta-analysis p-value.}
#'
#' @details
#' This function performs an inverse variance meta-analysis on grouped data.
#' It handles edge cases where standard errors are infinite or zero.
#'
#' @importFrom dplyr mutate summarise
#'
#' @export

run_inv_var <- function(
	grouped_dt, input_beta_name, input_se_name, 
	output_beta_meta, output_se_meta,
	output_meta_pvalue
) {
	dt <- grouped_dt %>% 
		mutate(weight = 1/(.data[[input_se_name]]**2)) %>%
		mutate(effs_inv_var = .data[[input_beta_name]] * weight) %>%
		summarise(
			"{output_beta_meta}" := sum(effs_inv_var) / sum(weight),
			"{output_se_meta}" := sqrt(1/sum(weight))) %>% 
		mutate("{output_meta_pvalue}" := 2 * pnorm(
				abs(.data[[output_beta_meta]] / .data[[output_se_meta]]), lower=FALSE))
	
	# Deal with the edge cases
	# Send the pvalues of the data with infinite standard errors to 1, and the 
	# pvalues of the data with standard errors of 0 to 0.
	 dt[[output_meta_pvalue]][is.na(dt[[output_meta_pvalue]]) & (dt[[output_se_meta]] == Inf)] <- 1
  	dt[[output_meta_pvalue]][is.na(dt[[output_meta_pvalue]]) & (dt[[output_se_meta]] == 0)] <- 0
  


	return(dt)
}
