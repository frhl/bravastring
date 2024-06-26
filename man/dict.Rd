% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dict.R
\name{dict}
\alias{dict}
\title{Create a Named Vector (Dictionary)}
\usage{
dict(keys, values)
}
\arguments{
\item{keys}{A character vector of keys.}

\item{values}{A vector of values corresponding to the keys.}
}
\value{
A named vector where the names are the keys and the elements are the values.
}
\description{
This function creates a named vector (dictionary) from two equal-length vectors of keys and values.
}
\examples{
keys <- c("a", "b", "c")
values <- c(1, 2, 3)
dictionary <- dict(keys, values)
print(dictionary)
# a b c 
# 1 2 3 
}
