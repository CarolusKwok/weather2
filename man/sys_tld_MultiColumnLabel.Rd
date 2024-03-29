% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_tool_data.R
\name{sys_tld_MultiColumnLabel}
\alias{sys_tld_MultiColumnLabel}
\title{System tools: Create a column of labels, pasted on character value of multiple columns}
\usage{
sys_tld_MultiColumnLabel(data, ..., name_as = "label", overwrite = F)
}
\arguments{
\item{data}{The dataframe itself}

\item{...}{The column names to form the labels}

\item{name_as}{Name of the new label. Default as \code{"grp"}. Accepts the keyword \code{"*del*"}}

\item{overwrite}{Let the new column names to overwrite the original dataframe columns? Default as \code{FALSE}.}
}
\description{
The columns of creating the label column will be passed from the \code{...} argument. Note that the \code{...} argument is not checked. Please make sure the columns are indeed present.
}
\examples{
tibble::tibble(a = c(1,2,3), b = c("a", "b", "c")) \%>\% sys_tld_MultiColumnLabel(a, b)
}
