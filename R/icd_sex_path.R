#' Get Path to BRAVA Phenotypes File
#'
#' This function returns the file path to the 'icd_sex_specific.txt' file included in the 'bravastring' package.
#'
#' @return A character string with the full path to the 'icd_sex_specific.txt' file.
#' @export
icd_sex_path <- function(){
  return(system.file('extdata/icd_sex_specific.txt', package = 'bravastring'))
}
