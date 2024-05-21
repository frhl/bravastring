#' Read and Process Biobank Results
#'
#' This function reads biobank results from a specified file produced via SAIGE step2, processes the data
#' to include effective sample size and allele counts, and selects and renames specific columns.
#'
#' @param path A character string specifying the file path to the biobank results.
#' @param suffix A character string to be appended to the selected column names.
#' @param EAC_cutoff A numeric value specifying the cutoff for the estimated allele count (EAC).
#'   Default is 0.
#' @return A data.table containing the processed biobank results with selected and renamed columns.
#' @examples
#' \dontrun{
#' library(data.table)
#' result <- read_biobank_result("path/to/biobank_results.txt", "_suffix")
#' print(result)
#' }
#' @import data.table
#' @import dplyr
#' @export
read_biobank_result <- function(path, suffix, EAC_cutoff=0) {
  d <- fread(path)
  cols_to_keep <- c("p.value", "SE", "BETA", "N_eff", "EAC", "CAF", "BIC")
  d <- add_N(d)
  d <- d %>%
    select(MarkerID, p.value, SE, BETA, N_eff, EAC, CAF, BIC) %>%
    rename_with(.fn = ~paste0(., suffix), .cols = cols_to_keep)
  return(d)
}
