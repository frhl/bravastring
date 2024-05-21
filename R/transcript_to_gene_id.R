#' Map Transcripts to Gene IDs
#'
#' This function maps a given set of transcript IDs to their corresponding gene IDs using the 'vep105_canonical.txt.gz' file.
#'
#' @param x A character vector of transcript IDs to be mapped to gene IDs.
#' @return A character vector of gene IDs corresponding to the input transcript IDs.
#' @examples
#' \dontrun{
#' transcript_ids <- c("ENST00000367770", "ENST00000494424")
#' gene_ids <- transcript_to_gene_id(transcript_ids)
#' print(gene_ids)
#' }
#' @import data.table
#' @export
transcript_to_gene_id <- function(x){
  d <- fread(canonical_path())
  map <- dict(d$TRANSCRIPT, d$GENE)
  return(as.character(map[x]))
}

