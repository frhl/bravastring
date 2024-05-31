#' Extract Biobank from a String
#'
#' This function extracts a biobank annotation from a given string using a regular expression pattern.
#' It constructs a regex pattern based on common variant annotations.
#'
#' @param x A character string from which to extract the biobank annotation.
#' @return A character string with the extracted biobank, or NA if no match is found.
#' @import stringr
#' @export
str_extract_annotation <- function(x){
  regex_pattern <- regex_vector(get_biobanks())
  return(stringr::str_extract(x, pattern=regex_pattern))
}

