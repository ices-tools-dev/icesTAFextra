#' Perform a clone from the ICES TAF GitHub organisation
#'
#' user must have approprate access privileges
#'
#' @param repo_name the name of the TAF repo you want to clone
#' @param local_dir the local directory you want to clone into
#'
#' @return a git2r repository object
#'
#' @importFrom git2r clone cred_token
#' @importFrom glue glue
#' @importFrom usethis github_token browse_github_token
#'
#' @examples
#'
#' \dontrun{
#' repo <- taf_clone("2019_TAF_template")
#' repo
#' }
#'
#' @export
taf_clone <-
  function(repo_name, local_dir = NULL) {

  if (github_token() == "") {
    browse_github_token()
    stop("Cannot proceed without a personal access token")
  }

  remote_url <- glue("https://github.com/ices-taf/{repo_name}.git")

  if (is.null(local_dir)) {
    local_dir <- repo_name
  }

  repo <-
    clone(
      remote_url,
      local_dir,
      credentials = cred_token(),
      branch = "master"
    )

  repo
}
