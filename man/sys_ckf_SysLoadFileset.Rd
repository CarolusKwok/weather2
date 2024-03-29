% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_check_fn.R
\name{sys_ckf_SysLoadFileset}
\alias{sys_ckf_SysLoadFileset}
\title{System tool: Check if the input satisfy the sys_load_fileset requirements}
\usage{
sys_ckf_SysLoadFileset(data, title, attempt, worker, list_fail, threshold)
}
\arguments{
\item{data}{Data frame containing columns "URL", "DIR", "Info", "Set"}

\item{title}{Title of the downloaded data}

\item{attempt}{Attempts to be made per download.}

\item{worker}{Numbers of sessions to be open}

\item{list_fail}{List failed-to-download items}

\item{threshold}{Threshold of line size. Any downloaded lines with number less than the threshold will be considered as failure and will be reattempted.}
}
\description{
System tool: Check if the input satisfy the sys_load_fileset requirements
}
\examples{
sys_ckf_SysLoadFileset(data, "title", 5L, 20L, TRUE, 1)
}
