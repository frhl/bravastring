#' Get Path to HGNC Ensembl Gene ID Map File
#'
#' This function returns the file path to the 'brava_pilot_phenotypes.txt' file included in the 'bravastring' package.
#'
#' @return A character string with the full path to the 'brava_pilot_phenotypes.txt' file.
#' @export
pilot_traits_path <- function(){
  return(system.file('extdata/brava_pilot_phenotypes.txt', package = 'bravastring'))
}


