% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{dpboardlog_update}
\alias{dpboardlog_update}
\title{Update dpboard log}
\usage{
dpboardlog_update(
  conf,
  git_info,
  dlog = NULL,
  dp_name = character(0),
  pin_version = character(0)
)
}
\arguments{
\item{conf}{output of \code{dpconf_get}}

\item{git_info}{a list returned from \code{gitinfo_validate}}

\item{dlog}{daap_log. This is only needed when adding record}

\item{dp_name}{name of the pin to be archived. Ignored when dlog is provided.}

\item{pin_version}{version of the pin to be archived.
Ignored when dlog is provided}
}
\value{
TRUE
}
\description{
Updates the metadata associated with the board and retrievable
with dp_list. When deploying dlog is needed when archiving dp_name and
pin_version are needed.
}
\keyword{internal}
