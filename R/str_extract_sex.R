#' Extract Sex Category
#'
#' This function extracts the sex category from a character string based on common sex category encodings.
#'
#' @param x A character string from which to extract the sex category.
#' @return A character string with the extracted sex category in uppercase, or NA if no match is found.
#' @examples
#' extracted_sex <- str_extract_sex("example string containing male sex category")
#' print(extracted_sex)
#' # [1] "MALE"
#' @import stringr
#' @export

str_extract_sex <- function(x){
  sex_categories <- get_sex_categories()
  regex_pattern <- paste0("\\b(", paste(sex_categories, collapse = "|"), ")\\b")
  matched_sex <- stringr::str_extract(x, pattern = regex(regex_pattern, ignore_case = TRUE))
  matched <- ifelse(is.na(matched_sex), NA, toupper(matched_sex))
  matched_sex[toupper(matched_sex)=="FEMALE"] <- "F"
  matched_sex[toupper(matched_sex)=="MALE"] <- "M"
  return(matched_sex)
}
