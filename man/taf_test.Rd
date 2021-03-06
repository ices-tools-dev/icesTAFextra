% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taf_test.R
\name{taf_test}
\alias{taf_test}
\title{Test TAF Repository}
\usage{
taf_test(repo_name, local_dir = NULL, overwrite = FALSE, interactive = FALSE)
}
\arguments{
\item{repo_name}{the name of a TAF repository.}

\item{local_dir}{the local directory to clone into.}

\item{overwrite}{whether to overwrite \code{local_dir} if it already exists.}

\item{interactive}{whether to show the log output in an editor/browser.}
}
\value{
A character vector containing the log output from running the analysis.
}
\description{
Clone and run the analysis of a repository from \verb{github.com/ices-taf} in
a local directory.
}
\note{
User must have approprate access privileges.
}
\examples{
\dontrun{
# Run a simple TAF example using local data
taf_test("2015_rjm-347d")

# Run a TAF example with bootstrap scripts
# and web services to fetch data
taf_test(
  "2019_TAF_template",
  local_dir = "mytest",
  overwrite = TRUE,
  interactive = TRUE
)
}

}
