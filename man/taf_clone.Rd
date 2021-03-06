% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taf_clone.R
\name{taf_clone}
\alias{taf_clone}
\title{Clone TAF Repository}
\usage{
taf_clone(repo_name, local_dir = NULL)
}
\arguments{
\item{repo_name}{the name of a TAF repository.}

\item{local_dir}{the local directory to clone into.}
}
\value{
A \verb{git2r} repository object.
}
\description{
Clone a repository from \verb{github.com/ices-taf} to a local directory.
}
\note{
User must have approprate access privileges.
}
\examples{
\dontrun{
repo <- taf_clone("2019_TAF_template")
repo
}

}
