#' Extract Number of Cases from a String
#'
#' This function extracts the number of cases from a given string using a regular expression pattern.
#' It looks for a number that appears before a dot and is surrounded by either dots or underscores
#' in file naming patterns like "UKB.EUR.ALL.4490.218383.CervCanc".
#'
#' @param x A character string from which to extract the number of cases.
#' @return A character string with the extracted number of cases, or NA if no match is found.
#' @examples
#' \dontrun{
#' extracted_cases <- str_extract_cases("UKB.EUR.ALL.4490.218383.CervCanc")
#' print(extracted_cases)
#' # [1] "4490"
#' }
#' @import stringr
#' @export
str_extract_cases <- function(x) {
  regex_pattern <- "(?<=[_\\.])[0-9]+(?=\\.\\d+[_\\.])"
  return(stringr::str_extract(x, pattern=regex_pattern))
}


