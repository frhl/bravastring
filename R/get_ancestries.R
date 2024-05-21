#' Get Ancestry Encodings
#'
#' This function returns a character vector of common ancestry encodings.
#'
#' @return A character vector containing the ancestry encodings: "eur", "eas", "sas", "afr", "jpn", "amr".
#' @examples
#' ancestries <- get_ancestries()
#' print(ancestries)
#' # [1] "eur" "eas" "sas" "afr" "jpn" "amr"
#' @export
get_ancestries <- function(){
  return(c("eur", "eas", "sas", "afr", "jpn", "amr"))
}
