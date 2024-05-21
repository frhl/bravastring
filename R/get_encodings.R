#' Get Genotype Encodings
#'
#' This function returns a character vector of common genotype encodings.
#'
#' @return A character vector containing the genotype encodings: "recessive", "additive", and "dominance".
#' @examples
#' encodings <- get_encodings()
#' print(encodings)
#' # [1] "recessive" "additive" "dominance"
#' @export
get_encodings <- function(){
  return(c("recessive", "additive", "dominance"))
}
