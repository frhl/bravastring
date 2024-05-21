#' Translate BRAVA Trait Identifier
#'
#' This function translates a BRAVA phenotype identifier to an alternative identifier format.
#' It reads the BRAVA phenotypes from a file and creates a mapping based on the identifiers.
#'
#' @param x A character vector of BRAVA phenotype identifiers to be translated.
#' @return A character vector of translated BRAVA phenotype identifiers.
#' @import data.table
#' @export
translate_brava_trait <- function(x){
  d <- fread(brava_trait_path())
  map_from_id <- any(x %in% d$phenotype_id)
  map_from_easy_id <- any(x %in% d$phenotype_id_easy)
  if ((map_from_id) & (map_from_easy_id)) stop("Multiple different mappings detected!")
  if (map_from_id){
    map <- dict(d$phenotype_id, d$phenotype_id_easy)
  } else if (map_from_easy_id){
    map <- dict(d$phenotype_id_easy, d$phenotype_id)
  } else {
    stop("No valid mapping found!")
  }
  return(as.character(map[x]))
}


