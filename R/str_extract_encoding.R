#' Extract Genotype Encoding from a String
#'
#' This function extracts a genotype encoding from a given string using a regular expression pattern.
#' It constructs a regex pattern based on common genotype encodings.
#'
#' @param x A character string from which to extract the genotype encoding.
#' @return A character string with the extracted genotype encoding, or NA if no match is found.
#' @examples
#' extracted_encoding <- str_extract_encoding("example string containing recessive")
#' print(extracted_encoding)
#' # [1] "recessive"
#' @import stringr
#' @export
str_extract_encoding <- function(x){
  regex_pattern <- regex_vector(get_encodings())
  return(stringr::str_extract(x, pattern=regex_pattern))
}
