% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_annotations.R
\name{get_annotations}
\alias{get_annotations}
\title{Get Variant Annotations}
\usage{
get_annotations()
}
\value{
A character vector containing the variant annotations: "pLoF_damaging_missense", "pLoF", "damaging_missense", "synonymous", and "nonsynonymous".
}
\description{
This function returns a character vector of common variant annotations.
}
\examples{
annotations <- get_annotations()
print(annotations)
# [1] "pLoF_damaging_missense" "pLoF" "damaging_missense" "synonymous" "nonsynonymous"
}
