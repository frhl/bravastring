#' Get Variant Annotations
#'
#' This function returns a character vector of common variant annotations.
#'
#' @return A character vector containing the variant annotations: "pLoF_damaging_missense", "pLoF", "damaging_missense", "synonymous", and "nonsynonymous".
#' @examples
#' annotations <- get_annotations()
#' print(annotations)
#' # [1] "pLoF_damaging_missense" "pLoF" "damaging_missense" "synonymous" "nonsynonymous"
#' @export
get_annotations <- function(){
  return(c("pLoF_damaging_missense", "pLoF", "damaging_missense", "synonymous", "nonsynonymous"))
}
