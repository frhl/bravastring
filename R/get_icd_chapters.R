#' Get ICD-10 Chapters and Code Ranges
#'
#' This function returns a list of ICD-10 chapters along with their corresponding code ranges.
#'
#' @return A named list where each name is a chapter and each value is the corresponding ICD-10 code range.
#' @examples
#' icd_chapters <- get_icd_chapters()
#' print(icd_chapters)
#' @export
get_icd_chapters <- function() {
  # Define the mapping of ICD-10 code ranges to chapters
  icd_chapters <- list(
    "A00-B99" = "Infectious",
    "C00-D49" = "Neoplasms",
    "D50-D89" = "Blood/immune",
    "E00-E89" = "Endocrine/metabolic",
    "F01-F99" = "Mental/behavioral",
    "G00-G99" = "Nervous",
    "H00-H59" = "Eye",
    "H60-H95" = "Ear",
    "I00-I99" = "Circulatory",
    "J00-J99" = "Respiratory",
    "K00-K95" = "Digestive",
    "L00-L99" = "Skin/subcutaneous",
    "M00-M99" = "Musculoskeletal",
    "N00-N99" = "Genitourinary",
    "O00-O9A" = "Pregnancy",
    "P00-P96" = "Congenital",
    "Z00-Z99" = "Health Factors"
  )
  
  return(icd_chapters)
}

