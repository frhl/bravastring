#' Create a Regular Expression Pattern from a Vector
#'
#' This function takes a vector of strings and concatenates them into a single regular expression pattern,
#' where each element is enclosed in parentheses and separated by the OR operator (|).
#'
#' @param x A character vector containing the strings to be concatenated into a regex pattern.
#' @return A single character string containing the concatenated regular expression pattern.
#' @examples
#' patterns <- c("apple", "banana", "cherry")
#' regex_pattern <- regex_vector(patterns)
#' print(regex_pattern)
#' # [1] "(apple)|(banana)|(cherry)"
#' @export
regex_vector <- function(x){
  x <- x[order(nchar(x), decreasing = TRUE)]
  paste(paste0("(", x, ")"), collapse = "|")
}


