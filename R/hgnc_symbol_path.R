#' Get Path to HGNC Ensembl Gene ID Map File
#'
#' This function returns the file path to the 'ensgid_hgnc_symbol_chrom.txt.gz' file included in the 'bravastring' package.
#'
#' @return A character string with the full path to the 'ensgid_hgnc_symbol_chrom.txt.gz' file.
#' @export
hgnc_symbol_path <- function(){
  return(system.file('extdata/ensgid_hgnc_symbol_chrom.txt.gz', package = 'bravastring'))
}


