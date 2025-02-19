% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_extract_cases.R
\name{str_extract_cases}
\alias{str_extract_cases}
\title{Extract Number of Cases from a String}
\usage{
str_extract_cases(x)
}
\arguments{
\item{x}{A character string from which to extract the number of cases.}
}
\value{
A character string with the extracted number of cases, or NA if no match is found.
}
\description{
This function extracts the number of cases from a given string using a regular expression pattern.
It looks for a number that appears before a dot and is surrounded by either dots or underscores
in file naming patterns like "UKB.EUR.ALL.4490.218383.CervCanc".
}
\examples{
\dontrun{
extracted_cases <- str_extract_cases("UKB.EUR.ALL.4490.218383.CervCanc")
print(extracted_cases)
# [1] "4490"
}
}
