% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/w2_load_fileset_stm.R
\name{w2_load_fileset_stm}
\alias{w2_load_fileset_stm}
\title{System tools: Download files from website using STORM mode}
\usage{
w2_load_fileset_stm(
  data,
  title = "test_set_stm",
  attempt = 5,
  worker = 20,
  threshold = 0.5,
  list_fail = T
)
}
\arguments{
\item{data}{Data frame containing columns "URL", "DIR", "Info", "Set"}

\item{title}{Title of the downloaded data}

\item{attempt}{Attempts to be made per download URL to be attempted.}

\item{worker}{Numbers of sessions to be open}

\item{threshold}{Threshold of file size, when compared to the largest item to be installed in the list}

\item{list_fail}{List failed-to-download items}
}
\value{

}
\description{
Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
Please use the w2_load_fileset() function instead.
}
\examples{
w2_load_fileset_stm(data, title = "Macao tidal data", attempt = 3, worker = 10, threshold = 0, list_fail = T)

}
