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
  three_digit_code <- substr(icd_code, 1, 3)
  
  # Define the mapping of three-digit ICD-10 codes to chapters
  icd_chapters <- c(
    A = "Infectious", B = "Infectious",
    C = "Neoplasms", D1 = "Neoplasms", D2 = "Blood/immune", 
    E = "Endocrine/metabolic",
    F = "Mental/behavioral",
    G = "Nervous",
    H0 = "Eye", H1 = "Eye", H2 = "Eye", H3 = "Eye", H4 = "Eye", H5 = "Eye", 
    H6 = "Ear", H7 = "Ear", H8 = "Ear", H9 = "Ear",
    I = "Circulatory",
    J = "Respiratory",
    K = "Digestive",
    L = "Skin/subcutaneous",
    M = "Musculoskeletal",
    N = "Genitourinary",
    O = "Pregnancy",
    P = "Congenital",
    Q = "Congenital",
    R = "Health Factors",
    S = "Injury", T = "Injury",
    V = "External causes", W = "External causes", X = "External causes", Y = "External causes", 
    Z = "Health Factors"
  )
  
  # Handle specific ranges for D codes
  if (three_digit_code %in% paste0("D", 0:49)) {
    chapter <- icd_chapters["D1"]
  } else if (three_digit_code %in% paste0("D", 50:89)) {
    chapter <- icd_chapters["D2"]
  } else {
    # Default lookup based on the first character
    first_char <- substr(three_digit_code, 1, 1)
    chapter <- icd_chapters[first_char]
  }
  
  return(as.character(chapter))
}


