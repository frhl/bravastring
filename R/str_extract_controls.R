#' Extract Number of Controls from a String
#'
#' This function extracts the number of controls from a given string using a regular expression pattern.
#' It looks for a number that appears after a dot and is surrounded by either dots or underscores
#' in file naming patterns like "UKB.EUR.ALL.4490.218383.CervCanc".
#'
#' @param x A character string from which to extract the number of controls.
#' @return A character string with the extracted number of controls, or NA if no match is found.
#' @examples
#' \dontrun{
#' extracted_controls <- str_extract_controls("UKB.EUR.ALL.4490.218383.CervCanc")
#' print(extracted_controls)
#' # [1] "218383"
#' }
#' @import stringr
#' @export
str_extract_controls <- function(x) {
    regex_pattern <- "\\.[0-9]+\\.([0-9]+)(?=[_\\.])"
    matches <- stringr::str_match(x, pattern = regex_pattern)
    return(matches[,2])  # Return the captured group
}

