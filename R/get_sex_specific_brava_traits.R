#' Get Sex-Specific Brava Traits
#'
#' Retrieves phenotype IDs for sex-specific Brava traits, optionally filtered by a single sex.
#'
#' @param use_new_names Logical, whether to use new phenotype names. Defaults to TRUE.
#' @param single_sex Optional character, filter traits by a single sex ("Female" or "Male").
#' @return A vector of phenotype IDs for traits that are not common to both sexes.
#' @import data.table
get_sex_specific_brava_traits <- function(use_new_names=TRUE, single_sex=NULL){
  d <- fread(brava_trait_path())
  if (!is.null(single_sex)){
    stopifnot(single_sex %in% c("Female", "Male"))
    d <- d[d$sex %in% single_sex,]
  }
  if (use_new_names){
    return(d$phenotype_id[tolower(d$sex) != "both"])
  } else {
    return(d$phenotype_id_easy[tolower(d$sex) != "both"])
  }
}

