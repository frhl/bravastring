#' Get Biobank Names
#'
#' This function returns a character vector of common biobank names.
#'
#' @return A character vector containing the biobank names: "UKB", "GEL", "GNH", "BBJ", "PMB", and "BioMe".
#' @examples
#' biobanks <- get_biobanks()
#' print(biobanks)
#' # [1] "UKB" "GEL" "GNH" "BBJ" "PMB" "BioMe". "AOU"
#' @export
get_biobanks <- function(){
  return(c("UKB", "GEL", "GNH", "BBJ", "PMB", "BioMe", "AOU"))
}
