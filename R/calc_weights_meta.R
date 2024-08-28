#' Calculate Weights for Meta-Analysis
#'
#' @param dt A data table or data frame.
#' @param is_inv_var Logical. If TRUE, use inverse variance weighting; if FALSE, use square root of effective sample size.
#' @param n_eff_name Character. Column name for effective sample size. Used when is_inv_var is FALSE.
#' @param se_name Character. Column name for standard errors. Used when is_inv_var is TRUE.
#'
#' @return Data table with added 'weights' column.
#' @export
#'

calc_weights_meta <- function(dt, is_inv_var=TRUE,
	n_eff_name=NULL, se_name=NULL) {
	if (is_inv_var) {
		return(dt %>% mutate(weights = 1/(.data[[se_name]]^2)))
	} else {
		return(dt %>% mutate(weights = sqrt(.data[[n_eff_name]])))
	}
}
