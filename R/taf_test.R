#' Test TAF Repository
#'
#' Clone and run the analysis of a repository from \verb{github.com/ices-taf} in
#' a local directory.
#'
#' @param repo_name the name of a TAF repository.
#' @param local_dir the local directory to clone into.
#' @param overwrite whether to overwrite \code{local_dir} if it already exists.
#' @param interactive whether to show the log output in an editor/browser.
#'
#' @return
#' A character vector containing the log output from running the analysis.
#'
#' @note User must have approprate access privileges.
#'
#' @importFrom git2r commits sha branch_create checkout workdir
#' @importFrom icesTAF taf.bootstrap sourceAll msg os
#' @importFrom tools file_path_as_absolute
#' @importFrom utils browseURL file.edit
#'
#' @examples
#' \dontrun{
#' # Run a simple TAF example using local data
#' taf_test("2015_rjm-347d")
#'
#' # Run a TAF example with bootstrap scripts
#' # and web services to fetch data
#' taf_test(
#'   "2019_TAF_template",
#'   local_dir = "mytest",
#'   overwrite = TRUE,
#'   interactive = TRUE
#' )
#' }
#'
#' @export

taf_test <- function(
  repo_name, local_dir = NULL, overwrite = FALSE,
  interactive = FALSE) {

  # only set to TRUE if interactive() is TRUE
  interactive <- interactive && interactive()

  if (is.null(local_dir)) {
    local_dir <-
      tempfile(
        pattern = repo_name
      )
  }

  if (dir.exists(local_dir) && !overwrite) {
    stop(
      "directory already exists, choose a different directory\n",
      "or set overwrite = TRUE"
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

  msg("running taf.bootstrap()")
  status_boot <-
    system2(
      "Rscript",
      args = "-e icesTAF::taf.bootstrap()",
      stdout = "log_boot.txt",
      stderr = "log_boot.txt"
    )
  if (status_boot > 0L) {
    warning("taf.bootstrap() had non-zero exit status", domain = NA)
  }

  msg("running sourceAll")
  status_source <-
    system2(
      "Rscript",
      args = "-e icesTAF::sourceAll()",
      stdout = "log_source.txt",
      stderr = "log_source.txt"
    )
  if (status_source > 0L) {
    warning("sourceAll() had non-zero exit status", domain = NA)
  }

  zz <- file("log_warnings.txt", open = "wt")
  sink(zz)
  sink(zz, type = "message")

  warnings()

  ## revert output back to the console -- only then access the file!
  sink(type = "message")
  sink()
  close(zz)

  # combine into one log file
  log <-
    c(
      "Bootstrap procedure",
      "===================\n",
      readLines("log_boot.txt"),
      "\n\nSourcing TAF scripts",
      "====================\n",
      readLines("log_source.txt"),
      "\n\nList of warnings",
      "================\n",
      readLines("log_warnings.txt")
    )
  cat(log, file = "log.txt", sep = "\n")
  unlink("log_boot.txt")
  unlink("log_source.txt")
  unlink("log_warnings.txt")

  setwd(od)


  # process log file for errors and warnings

  # process any json or xml data exports

  # check R code in sourceAll does not contact the internet

  msg("Results in:\n", file_path_as_absolute(local_dir))
  if (os() == "Windows" && interactive) {
    browseURL(
      paste0("file://", file_path_as_absolute(local_dir))
    )
  }
  if (interactive) {
    file.edit(file.path(file_path_as_absolute(local_dir), "log.txt"))
  }
  invisible(log)
}
