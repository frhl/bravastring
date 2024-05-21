#' Extract Ancestry Annotation
#'
#' This function extracts the ancestry annotation from a character string based on common ancestry encodings.
#'
#' @param x A character string from which to extract the ancestry annotation.
#' @return A character string with the extracted ancestry annotation, or NA if no match is found.
#' @examples
#' extracted_ancestry <- str_extract_ancestry("example string containing eur ancestry")
#' print(extracted_ancestry)
#' # [1] "eur"
#' @import stringr
#' @export
str_extract_ancestry <- function(x){
  regex_pattern <- regex_vector(get_ancestries())
  return(stringr::str_extract(x, pattern = regex_pattern))
}

