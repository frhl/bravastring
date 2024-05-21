#' Extract Variant Annotation from a String
#'
#' This function extracts a variant annotation from a given string using a regular expression pattern.
#' It constructs a regex pattern based on common variant annotations.
#'
#' @param x A character string from which to extract the variant annotation.
#' @return A character string with the extracted variant annotation, or NA if no match is found.
#' @examples
#' extracted_annotation <- str_extract_annotation("example string containing pLoF_damaging_missense")
#' print(extracted_annotation)
#' # [1] "pLoF_damaging_missense"
#' @import stringr
#' @export
str_extract_annotation <- function(x){
  regex_pattern <- regex_vector(get_annotations())
  return(stringr::str_extract(x, pattern=regex_pattern))
}

