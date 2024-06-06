#' Get Path to BRAVA Phenotypes File
#'
#' This function returns the file path to the 'brava_main_pilot_n_eff.txt' file included in the 'bravastring' package.
#'
#' @return A character string with the full path to the 'brava_main_pilot_n_eff.txt' file.
#' @examples
#' brava_file_path <- brava_trait_path()
#' print(brava_file_path)
#' @export
brava_samples_path <- function(){
  return(system.file('extdata/brava_main_pilot_n_eff.txt', package = 'bravastring'))
}
