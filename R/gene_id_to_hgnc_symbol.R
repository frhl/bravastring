#' Map Transcripts to Gene IDs to HGNC symbols
#'
#' This function maps a given set of esnembl gene IDs to their corresponding hgnc_symbols.
#'
#' @param x A character vector of transcript IDs to be mapped to gene IDs.
#' @return A character vector of hgnc IDs corresponding to the input ensembl gene IDs.
#' @import data.table
#' @export
gene_id_to_hgnc_symbol <- function(x){
  d <- fread(hgnc_symbol_path())
  map <- dict(d$ensembl_gene_id, d$hgnc_symbol)
  return(as.character(map[x]))
}


