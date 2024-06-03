#' Add Effective Sample Size and Allele Counts to Variant Data
#'
#' This function adds effective sample size (N_eff), estimated allele count (EAC), 
#' combined allele frequency (CAF), and the number of individuals carrying the 
#' alternative allele (BIC) to a data.table of variant data. It handles both binary 
#' and continuous traits.
#'
#' @param dt_variant A data.table containing variant data. Should contain columns 
#'   for either 'N' or both 'N_case' and 'N_ctrl', as well as 'AF_Allele2' and 
#'   'AC_Allele2'.
#' @return The input data.table \code{dt_variant} with additional columns: 
#'   \code{N_eff}, \code{EAC}, \code{CAF}, and \code{BIC}.
#' @examples
#' \dontrun{
#' library(data.table)
#' dt_variant <- data.table(
#'   N_case = c(1000, 1200, 1100),
#'   N_ctrl = c(2000, 2200, 2100),
#'   AF_Allele2 = c(0.1, 0.2, 0.15),
#'   AC_Allele2 = c(300, 480, 330)
#' )
#' result <- add_N(dt_variant)
#' print(result)
#' }
#' @import data.table
#' @export
add_N <- function(dt_variant) {
  if ("N" %in% colnames(dt_variant)) {
    N_eff <- mean(dt_variant$N)
    dt_variant$N_eff <- N_eff
    binary <- FALSE
  } else {
    N_case <- mean(dt_variant$N_case)
    N_control <- mean(dt_variant$N_ctrl)
    N_eff <- unique((4 * N_case * N_control) / (N_case + N_control))
    dt_variant$N_eff <- N_eff
    dt_variant$N_case <- N_case
    dt_variant$N_control <- N_control
    binary <- TRUE
  }
  
  return(dt_variant)
}

