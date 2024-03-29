% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_check_list.R
\name{sys_ckl_length}
\alias{sys_ckl_length}
\title{System tool: Check if a list/vector has a certain length}
\usage{
sys_ckl_length(list, list_name, expected, mode = "==", silent = F)
}
\arguments{
\item{list}{The list}

\item{list_name}{The name of the list}

\item{expected}{Expected length of the list. Must be an integer}

\item{mode}{Mode of comparison. Default as \code{==}
\itemize{
\item "==" (equal to)
\item "!=" (not equal to)
\item ">" (larger than)
\item "<" (smaller than)
\item ">=" (larger or equal to)
\item "<=" (smaller or equal to)
}}

\item{silent}{Should a text message be shown if incorrect? Default as False.}
}
\description{
System tool: Check if a list/vector has a certain length
}
\examples{
sys_ckl_length(c(1,2,3), "obj", 2L) #Return `TRUE`
}
