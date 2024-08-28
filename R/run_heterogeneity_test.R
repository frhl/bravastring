#' Perform Heterogeneity Test on Grouped Data
#'
#' This function conducts a heterogeneity test on grouped data, calculating
#' a meta-analysis beta coefficient and associated heterogeneity statistics.
#'
#' @param grouped_dt A grouped data table or data frame. It should contain
#'   columns for weights and the input beta coefficients.
#' @param input_beta Character string. Name of the column containing input beta coefficients.
#' @param output_meta_beta Character string. Name to be used for the output meta-analysis beta coefficient column.
#'
#' @return A data table with the following columns:
#'   \item{df}{Degrees of freedom (number of groups minus 1).}
#'   \item{sum_weights}{Sum of the weights.}
#'   \item{output_meta_beta}{The calculated meta-analysis beta coefficient.}
#'   \item{chisq_het}{Chi-square statistic for heterogeneity.}
#'   \item{Pvalue_het}{P-value for the heterogeneity test.}
#'
#' @details
#' The function calculates a weighted average of the input beta coefficients to produce
#' a meta-analysis beta. It then computes a chi-square statistic to test for 
#' heterogeneity among the input betas. The p-value for this test is calculated 
#' using the chi-square distribution with (n-1) degrees of freedom, where n is 
#' the number of groups.
#'
#' @importFrom dplyr mutate summarise

run_heterogeneity_test <- function(
	grouped_dt, input_beta, output_meta_beta) {
	grouped_dt %>% 
	mutate(weighted_effects = weights * .data[[input_beta]]) %>% 
	summarise(
		df = n()-1,
		sum_weights = sum(weights),
		"{output_meta_beta}" := sum(weighted_effects) / sum_weights,
		chisq_het = sum(
			weights * (.data[[input_beta]] - .data[[output_meta_beta]])^2),
		Pvalue_het = pchisq(chisq_het, df, lower.tail=FALSE)
	)
}

