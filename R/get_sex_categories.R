#' Get Sex Categories
#'
#' This function returns a character vector of common sex categories.
#'
#' @return A character vector containing the sex categories: "ALL", "FEMALE", "MALE", "M", "F", and "BOTH".
#' @examples
#' sex_categories <- get_sex_categories()
#' print(sex_categories)
#' # [1] "ALL" "FEMALE" "MALE" "M" "F" "BOTH"
#' @export
get_sex_categories <- function(){
  return(c("ALL", "FEMALE", "MALE", "M", "F", "BOTH"))
}
