% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dpinput_sync.R
\name{pathnames_reroot}
\alias{pathnames_reroot}
\title{Re-root path names}
\usage{
pathnames_reroot(pathnames, new_root = "input_files")
}
\arguments{
\item{pathnames}{a vector of characters to be re-rooted}

\item{new_root}{a directory relative to which all paths be renamed}
}
\description{
if pathnames are of path format, it sets the root to \code{new_root}
dropping all upstream paths beyond \code{new_root}. If not of path format, it
keeps the pathnames unchanged
}
\keyword{internal}
