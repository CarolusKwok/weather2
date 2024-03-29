% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_smooth.R
\name{calc_smooth_lm}
\alias{calc_smooth_lm}
\title{Calculate by smoothing and filling the columns, with linear models}
\usage{
calc_smooth_lm(
  data,
  based,
  value,
  max_fill = NULL,
  trailing = T,
  name_as = NULL,
  overwrite = F
)
}
\arguments{
\item{data}{The dataframe itself, storing all observations. The dataframe must not be grouped.}

\item{based}{The column name of how the data should be arranged, in ascending order. This column must not contain NAs, must be unique, and must be "numericable".}

\item{value}{The column name of what values should be smoothed. This column must contain NAs.}

\item{max_fill}{The maximum size of a NA-gap. Default as \code{NULL}, which ignores this argument. Must be numeric.}

\item{trailing}{Remove all NAs at the head of prediction column? Default as \code{TRUE}.}

\item{name_as}{Names of the 1 new columns, i.e. the newly smoothed data. Default as \code{NULL}, i.e. the column name of \code{value} with a prefix of \code{"slm_"}. Keyword \code{"*del*"} is supported.}

\item{overwrite}{Let the new column names to overwrite the original dataframe columns? Default as \code{FALSE}.}
}
\value{
The same dataframe as \code{data}, with 1 additional column with column name defined in \code{name_as}.
}
\description{
Data smoothing is an important process in data analysis to fill up missing data, or to remove extreme values.
This function fills up missing data with linear model, predicted by the head and tail of the gap.
}
\examples{
calc_smooth_lm(data, x, y, trailing = F)
}
