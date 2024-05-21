#' Clean and Standardize Column Names
#'
#' This function takes a vector of column names and returns a cleaned version of these names.
#' It standardizes the names by converting them to lowercase, replacing spaces, commas, and hyphens
#' with underscores, removing certain special characters, and replacing some symbols with text equivalents.
#'
#' @param column_names A character vector containing the column names to be cleaned.
#' @return A character vector with the cleaned and standardized column names.
#' @examples
#' column_names <- c("Column 1", "Column-2", "Column, 3", "Column&4")
#' clean_column_names <- clean_string(column_names)
#' print(clean_column_names)
#' # [1] "column_1" "column_2" "column_3" "column_or_4"
#' @export
clean_string <- function(column_names) {
  cleaned_names <- tolower(gsub("(,)|(\\ )|(\\-)", "_", column_names))
  cleaned_names <- gsub("(\\[)|(\\])|(\\()|(\\))|(\\.)|(\\*)", "", cleaned_names)
  cleaned_names <- gsub("(\\&)|(\\/)", "_or_", cleaned_names)
  cleaned_names <- gsub("_+", "_", cleaned_names)
  return(cleaned_names)
}
