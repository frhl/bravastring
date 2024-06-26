% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_extract_sex.R
\name{str_extract_sex}
\alias{str_extract_sex}
\title{Extract Sex Category}
\usage{
str_extract_sex(x)
}
\arguments{
\item{x}{A character string from which to extract the sex category.}
}
\value{
A character string with the extracted sex category in uppercase, or NA if no match is found.
}
\description{
This function extracts the sex category from a character string based on common sex category encodings.
}
\examples{
extracted_sex <- str_extract_sex("example string containing male sex category")
print(extracted_sex)
# [1] "MALE"
}
