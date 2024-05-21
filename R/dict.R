#' Create a Named Vector (Dictionary)
#'
#' This function creates a named vector (dictionary) from two equal-length vectors of keys and values.
#'
#' @param keys A character vector of keys.
#' @param values A vector of values corresponding to the keys.
#' @return A named vector where the names are the keys and the elements are the values.
#' @examples
#' keys <- c("a", "b", "c")
#' values <- c(1, 2, 3)
#' dictionary <- dict(keys, values)
#' print(dictionary)
#' # a b c 
#' # 1 2 3 
#' @export
dict <- function(keys, values){
  stopifnot(length(keys) == length(unique(keys)))
  out <- values
  names(out) <- keys
  return(out)
}
