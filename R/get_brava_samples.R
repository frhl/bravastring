#' Get Brava Samples
#'
#' This function retrieves the effective sample size for a given biobank, ancestry, trait, and optionally sex.
#'
#' @param biobank Character. The name of the biobank.
#' @param ancestry Character. The ancestry of the samples.
#' @param trait Character. The trait of interest.
#' @param sex Character. The sex of the samples. Can be "F" (Female), "M" (Male), or "ALL" (default is NULL, which infers sex based on the trait).
#' @param column Character. The column to retrieve from the data (default is "N_eff").
#' @return Numeric vector. The effective sample size for the specified parameters.
#' @examples
#' \dontrun{
#' get_brava_samples("ukb", "EUR", "Height")
#' get_brava_samples("ukb", "EUR", "Height", sex = "F")
#' }
#' @export

get_brava_samples <- function(biobank, ancestry, trait, sex=NULL, column="N_eff"){
  
  # process input
  the_trait <- trait
  the_biobank <- toupper(biobank)
  the_ancestry <- toupper(ancestry)
  
  # get sex specific traits
  if (is.null(sex)){
    female_traits <- get_sex_specific_brava_traits(single_sex = "Female")
    male_traits <- get_sex_specific_brava_traits(single_sex = "Male")
    if (trait %in% female_traits){
      the_sex <- "F"
    } else if (trait %in% male_traits){
      the_sex <- "M"
    } else {
      the_sex <- "ALL"
    }
  } else {
    the_sex <- toupper(sex)
  }

  # process reference file
  d <- fread(brava_samples_path())
  stopifnot(column %in% colnames(d))
  d$biobank <- toupper(d$biobank)
  d$ancestry <- toupper(d$ancestry)
 
  if (!the_biobank %in% d$biobank) stop(paste("valid biobanks:",paste(unique(d$biobank),collapse="|")))
  if (!the_ancestry %in% d$ancestry) stop(paste("valid ancestry:",paste(unique(d$ancestry),collapse="|")))
  if (!the_trait %in% d$trait) stop(paste("valid traits:",paste(unique(d$trait),collapse="|")))
  
  # process the file
  d <- d[(d$biobank %in% the_biobank) & (d$trait %in% the_trait),]
  if (nrow(d) == 0 ) stop(paste(the_trait, "does not exists in", the_biobank))
  d <- d[d$ancestry %in% the_ancestry, ]
  if (nrow(d) == 0 ) stop(paste(the_trait, "does not exists in", the_biobank, "for ancestry", the_ancestry))
  d <- d[d$sex %in% the_sex, ]
  if (nrow(d) == 0 ) stop(paste(the_trait, "does not exists in", the_biobank, "for ancestry", the_ancestry, "with sex", the_sex))
  
  # return relevant column
  return(d[[column]])
  
  
}

