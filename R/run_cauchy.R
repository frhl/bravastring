#' Apply Cauchy Combination Test on Grouped Data
#'
#' This function performs a Cauchy combination test on grouped data, calculating
#' a meta-analysis p-value based on input p-values and sample sizes.
#'
#' @param grouped_dt A grouped data table or data frame.
#' @param n_eff_name Character string. Name of the column containing effective sample sizes.
#' @param Cauchy_stat_name Character string. Name to be used for the output Cauchy statistic column.
#' @param input_pvalues Character string. Name of the column containing input p-values.
#' @param output_meta_pvalue Character string. Name to be used for the output meta-analysis p-value column.
#'
#' @return A data table with added columns:
#'   \item{Cauchy_stat_name}{The calculated Cauchy statistic.}
#'   \item{number_of_pvals}{The number of input p-values used.}
#'   \item{output_meta_pvalue}{The calculated meta-analysis p-value.}
#'
#' @details
#' The function uses the Cauchy combination method to combine p-values, weighted
#' by the square root of the effective sample sizes. It handles extreme values 
#' and adjusts the final p-value to avoid values too close to 1.
#'
#' @importFrom dplyr summarise mutate
#'
#' @export


run_cauchy <- function(
	grouped_dt, n_eff_name, Cauchy_stat_name, input_pvalues, output_meta_pvalue) 
{
	return(
		grouped_dt %>% 
		summarise(
			"{Cauchy_stat_name}" := cauchy_combination(
				.data[[input_pvalues]],
				weights=sqrt(.data[[n_eff_name]])
			),
			number_of_pvals := n()
		) %>% mutate(
		  	"{output_meta_pvalue}" := ifelse(
				.data[[Cauchy_stat_name]] > 1e+15,
				(1 / .data[[Cauchy_stat_name]]) / pi,
				pcauchy(.data[[Cauchy_stat_name]], lower.tail=FALSE)
			)
		) %>% mutate(
			"{output_meta_pvalue}" := ifelse(
				.data[[output_meta_pvalue]] > (1 - 1e-10),
				(1 - 1/number_of_pvals), .data[[output_meta_pvalue]]
			)
		)
	)
}
