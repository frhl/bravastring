#' Check if Transcript is Canonical
#'
#' This function checks if a given transcript is canonical based on the 'vep105_canonical.txt.gz' file.
#'
#' @param x A character vector of transcript IDs to check for canonicity.
#' @return A logical vector indicating whether each transcript ID is canonical.
#' @examples
#' \dontrun{
#' transcript_ids <- c("ENST00000367770", "ENST00000494424")
#' canonical_status <- is_canonical(transcript_ids)
#' print(canonical_status)
#' }
#' @import data.table
#' @export
is_canonical <- function(x){
  d <- fread(canonical_path())
  d <- d[d$CANONICAL == TRUE,]
  return(x %in% d$TRANSCRIPT)
}

