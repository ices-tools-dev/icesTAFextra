#' Perform a test run of a TAF repository
#'
#' user must have approprate access privileges
#'
#' @param repo_name the name of the TAF repo you want to clone
#' @param local_dir the local directory you want to clone into
#' @param overwrite if local_dir exists, should it be overwritten
#'
#' @return a git2r repository object
#'
#' @importFrom git2r commits sha branch_create checkout workdir
#' @importFrom icesTAF taf.bootstrap sourceAll msg
#' @importFrom tools file_path_as_absolute
#'
#' @examples
#'
#' \dontrun{
#' taf_test(
#'   "2019_TAF_template",
#'   local_dir = "mytest",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
taf_test <- function(repo_name, local_dir = NULL, overwrite = FALSE) {
  if (is.null(local_dir)) {
    local_dir <-
      tempfile(
        pattern = repo_name
      )
  }

  if (dir.exists(local_dir) && !overwrite) {
    stop(
      "directory already exists, choose a different directory\n",
      "or set, overwrite = TRUE"
    )
  }
  if (dir.exists(local_dir) && overwrite) {
    unlink(local_dir, recursive = TRUE, force = TRUE)
  }

  repo <- taf_clone(repo_name, local_dir)

  # create a new branch to run in, just in case
  head <- commits(repo, ref = "refs/heads/master")[[1]]

  branch_name <- paste0("taf-", substring(sha(head), 1, 10))
  branch_create(head, branch_name)
  checkout(repo, branch_name)

  od <- setwd(workdir(repo))
  on.exit(setwd(od))

  zz <- file("log.txt", open = "wt")
  sink(zz)
  sink(zz, type = "message")

  log <- ""
  try(
    log <-
      c(
        log,
        system("Rscript -e icesTAF::taf.bootstrap()", intern = TRUE)
      )
  )
  try(
    log <-
      c(
        log,
        system("Rscript -e icesTAF::sourceAll()", intern = TRUE)
      )
  )

  warnings()

  ## revert output back to the console -- only then access the file!
  sink(type = "message")
  sink()
  close(zz)

  setwd(od)

  # process log file?

  msg("results in ", local_dir)
  browseURL(
    paste0("file://", file_path_as_absolute("mytest"))
  )
  invisible(log)
}
