#' Extract BRAVA Trait from a String using GEL Biobank
#'
#' This function extracts a BRAVA phenotype identifier from a given string using a regular expression pattern.
#' It reads the BRAVA phenotypes from a file and constructs a regex pattern based on the identifiers.
#'
#' @param x A character string from which to extract the BRAVA phenotype identifier.
#' @return A character string with the extracted BRAVA phenotype identifier, or NA if no match is found.
#' @import data.table
#' @import stringr
#' @export
str_extract_brava_trait_gel <- function(x){
  brava <- fread(brava_trait_path())
  regex_pattern <- regex_vector(brava$phenotype_id_easy)
  return(stringr::str_extract(x, pattern=regex_pattern))
}
