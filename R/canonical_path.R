#' Get Path to Canonical Transcripts File
#'
#' This function returns the file path to the 'vep105_canonical.txt.gz' file included in the 'bravastring' package.
#'
#' @return A character string with the full path to the 'vep105_canonical.txt.gz' file.
#' @examples
#' canonical_file_path <- canonical_path()
#' print(canonical_file_path)
#' @export
canonical_path <- function(){
  return(system.file('extdata/vep105_canonical.txt.gz', package = 'bravastring'))
}

