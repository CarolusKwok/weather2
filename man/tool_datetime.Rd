% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tool_datetime.R
\name{tool_datetime}
\alias{tool_datetime}
\title{Tool: To create a sequence of date time}
\usage{
tool_datetime(start, end, tz = Sys.timezone(), by, duration)
}
\arguments{
\item{start}{Starting time, can be in the format of Date or POSIXct.}

\item{end}{Ending time, can be in the format of Date or POSIXct.}

\item{tz}{Time zone of start and end. Must be a string.}

\item{by}{Time interval of the sequence. Must be a string.}

\item{duration}{Duration between starting time and ending time. Must be a string. Overrides when both start and end is supplied.}
}
\description{
Tool: To create a sequence of date time
}
\examples{
tool_datetime(end = Sys.time(), by = "2 hours", duration = "1 day")
}
