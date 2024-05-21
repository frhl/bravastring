#' Extract BRAVA Trait from a String
#'
#' This function extracts a BRAVA phenotype identifier from a given string using a regular expression pattern.
#' It reads the BRAVA phenotypes from a file and constructs a regex pattern based on the identifiers.
#'
#' @param x A character string from which to extract the BRAVA phenotype identifier.
#' @param use_latest_names A logical value indicating whether to use the latest phenotype identifiers (default is TRUE).
#' If FALSE, uses an alternative set of identifiers.
#' @return A character string with the extracted BRAVA phenotype identifier, or NA if no match is found.
#' @examples
#' extracted_trait <- str_extract_brava_trait("example string containing phenotype identifier")
#' print(extracted_trait)
#' @import data.table
#' @import stringr
#' @export
str_extract_brava_trait <- function(x, use_latest_names=TRUE){
  brava <- fread(brava_trait_path())
  if (use_latest_names){
    regex_pattern <- regex_vector(brava$phenotype_id)
  } else {
    regex_pattern <- regex_vector(brava$phenotype_id_easy)
  }
  return(stringr::str_extract(x, pattern=regex_pattern))
}
