% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_load_line.R
\name{sys_load_line}
\alias{sys_load_line}
\title{System tools: Download lines from website}
\usage{
sys_load_line(
  data,
  title = "test",
  attempt = 5,
  worker = 0,
  list_fail = T,
  threshold = 1,
  check = T
)
}
\arguments{
\item{data}{Data frame containing columns \code{URL} and \code{Info}.}

\item{title}{Title of the downloaded data}

\item{attempt}{Attempts to be made per download URL to be attempted. Default as a maximum of \code{5} times.}

\item{worker}{Numbers of sessions to be open.}

\item{list_fail}{List failed-to-download items. Default as \code{TRUE}.}

\item{threshold}{Threshold of line size. Any downloaded lines with number less than the threshold will be considered as failure and will be reattempted. Default as a minimum of \code{1} line.}

\item{check}{Check the all of the parameters before execution. Default as \code{TRUE}.}
}
\description{
System tools: Download lines from website
}
\examples{
sys_load_line(data, title = "Wyoming Atmospheric Sounding Data")
}
