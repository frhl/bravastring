#' Map ICD-10 Codes to Chapters
#'
#' This function maps two-digit ICD-10 codes to their corresponding chapters based on the predefined ranges.
#'
#' @param icd_code A character string representing the ICD-10 code.
#' @return A character string representing the chapter name corresponding to the given ICD-10 code.
#' @examples
#' map_icd_to_chapter("A15") # Should return "Infectious"
#' map_icd_to_chapter("C34") # Should return "Neoplasms"
#' @export
map_icd_to_chapter <- function(icd_code) {
  # Ensure the input is a character string
  icd_code <- as.character(icd_code)
  
  # Extract the first three characters of the ICD-10 code to handle ranges like "A00-B99"
  two_digit_code <- substr(icd_code, 1, 3)
  
  # Define the mapping of two-digit ICD-10 codes to chapters
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
  
  # Helper function to compare ICD-10 codes
  icd_in_range <- function(code, range) {
    start <- substr(range, 1, 3)
    end <- substr(range, 5, 7)
    return(code >= start && code <= end)
  }
  
  # Function to find the corresponding chapter for a given two-digit code
  find_chapter <- function(code) {
    for (range in names(icd_chapters)) {
      if (icd_in_range(code, range)) {
        return(icd_chapters[[range]])
      }
    }
    return(NA) # Return NA if the code does not match any chapter
  }
  
  # Apply the find_chapter function to the two-digit code
  chapter <- find_chapter(two_digit_code)
  
  return(chapter)
}
